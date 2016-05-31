/**
 * Copyright (c) 2016 Structured Data LLC
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * The structure and some of the methods in this file are based on 
 *
 * https://github.com/hadley/svglite
 *
 * which is copyright
 *
 *  (C) 2002 T Jake Luciani: SVG device, based on PicTex device
 *  (C) 2008 Tony Plate: Line type support from RSVGTipsDevice package
 *  (C) 2012 Matthieu Decorde: UTF-8 support, XML reserved characters and XML header
 *  (C) 2015 RStudio (Hadley Wickham): modernisation & refactoring
 *
 */
 
#include <Rcpp.h>
#include <gdtools.h>
#include <R_ext/GraphicsEngine.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>

#include <iostream>
#include <sstream>
#include <string>

#define PACKET_END "\n"

using namespace Rcpp;
using namespace std;

typedef void * CALLBACK_FN_JSON( const char *, const char *, bool );
typedef void * CALLBACK_FN_SEXP( const char *, SEXP, bool );
typedef SEXP CALLBACK_FN_SYNC( SEXP, bool );

class JSGraphicsDevice {
public:
	CALLBACK_FN_JSON *callback;
	XPtrCairoContext cc;
	int page;
  	int device;
		  
public:

	JSGraphicsDevice() : 
		page(0), 
		device(0),
		callback(0),
		cc(gdtools::context_create()) {
			callback = (CALLBACK_FN_JSON*)R_GetCCallable("ControlR", "CallbackJSON");
			if( !callback ){
				cout << "ERR: can't find callback pointer" << endl;
			}
			else {
				this->write( "{ \"cmd\": \"status\", \"data\": { \"msg\": \"registered graphics device\" }}" );
			}
	}
	
	~JSGraphicsDevice(){}

public:

	void setDevice( int id ){
		device = id;
	}

	void write( const char *data ){
		if( callback ) callback( "graphics", data, true );
	}

};

inline bool is_filled(int col) {
	const int alpha = R_ALPHA(col);
	return (col != NA_INTEGER) && (alpha != 0);
}

inline bool is_bold(int face) { return face == 2 || face == 4; }

inline bool is_italic(int face) { return face == 3 || face == 4; }

inline std::string fontname(const char* family_, int face) {
	
	std::string family(family_);
	if (face == 5) return "symbol";

#ifdef WIN32

	if (family == "mono") {
		return "Consolas"; 
	} 
	else if (family == "serif") {
		return "Palatino";
	} 
	else if (family == "sans" || family == "") {
		return "Segoe UI"; 
	} 
	else {
		return family;
	}

#elif __APPLE__

	if( family == "mono" ) {
		return "Menlo";
	}
	else if (family == "serif") {
		return "Georgia";
	} 
	else if (family == "sans" || family == "") {
		return "Helvetica Neue"; 
	} 
	else {
		return family;
	}

#else // #ifdef WIN32

	if (family == "mono") {
		return "Monospace"; 
	} 
	else if (family == "serif") {
		return "Serif";
	} 
	else if (family == "sans" || family == "") {
		return "Sans Serif"; 
	} 
	else {
		return family;
	}

#endif // #ifdef WIN32	
    
}

const char *rgba( rcolor col ){

	static char color_buffer[64];
	static const char *none = "none";

	int alpha = R_ALPHA(col);

	if (col == NA_INTEGER || alpha == 0) {
		return none;
	} 
	if( alpha == 255.0 ){
		sprintf( color_buffer, "rgb(%d,%d,%d)", 
		R_RED(col), R_GREEN(col), R_BLUE(col));
	}
	else {	  
		sprintf( color_buffer, "rgba(%d,%d,%d,%01.02f)", 
			R_RED(col), R_GREEN(col), R_BLUE(col), alpha/255.0 );
	}
	return color_buffer;	  

}

const char * write_style_linetype( const pGEcontext gc, int filled) {

	static char buffer[512];
	static char fill[64];

	const char BUTT[] = "butt";
	const char ROUND[] = "round";
	const char BEVEL[] = "bevel";
	const char MITRE[] = "mitre";
	const char SQUARE[] = "square";
	
	const char *pcap = ROUND;
	const char *pjoin = ROUND;

	switch(gc->lend)
	{
	case GE_BUTT_CAP: pcap = BUTT; break;
	case GE_SQUARE_CAP: pcap = SQUARE; break;
	}

	switch(gc->ljoin)
	{
	case GE_BEVEL_JOIN: pjoin = BEVEL; break;
	case GE_MITRE_JOIN: pjoin = MITRE; break;
	}

	if( filled && is_filled(gc->fill)){
		sprintf( fill, ", \"fill\": \"%s\"", rgba(gc->fill) );
	}
	else fill[0] = 0;

	sprintf( buffer, "{ \"linewidth\": %01.02f, \"color\": \"%s\", \"cap\": \"%s\", \"join\": \"%s\"%s}", 
		gc->lwd / 96 * 72,
		rgba(gc->col),
		pcap, pjoin, fill );

	return buffer;
  
}

void get_metric_info( int c, const pGEcontext gc, double* ascent,
						double* descent, double* width, pDevDesc dd) {

	JSGraphicsDevice *pd = (JSGraphicsDevice*)dd->deviceSpecific;						 

	// Convert to string - negative implies unicode code point
	char str[16];
	if (c < 0) {
		Rf_ucstoutf8(str, (unsigned int) -c);
	} else {
		str[0] = (char) c;
		str[1] = '\0';
	}

	gdtools::context_set_font(pd->cc, fontname(gc->fontfamily, gc->fontface),
		gc->cex * gc->ps, is_bold(gc->fontface), is_italic(gc->fontface));
	FontMetric fm = gdtools::context_extents(pd->cc, std::string(str));

	*ascent = fm.ascent;
	*descent = fm.descent;
	*width = fm.width;

}

void set_clip(double x0, double x1, double y0, double y1, pDevDesc dd) {

}
 
void new_page(const pGEcontext gc, pDevDesc dd) {
	
	// does this command reset the display list? 
	// it might be useful to capture a snapshot
	// _before_ it's wiped...
	
	JSGraphicsDevice *pd = (JSGraphicsDevice*)dd->deviceSpecific;						 

	std::ostringstream os;
	
	os << "{ \"cmd\": \"new-page\", \"device\": " << pd->device << ", \"data\": { \"page\": "
		<< pd->page << ", \"height\": "
		<< fixed << (double)(dd->bottom) << ", \"width\": "
		<< fixed << (double)(dd->right) << ", \"background\": \""
		<< rgba(dd->startfill) << "\" }}" ;

	pd->write(os.str().c_str());

	// increment for next call
	pd->page++;

}

void close_device(pDevDesc dd) {
	
	JSGraphicsDevice *pd = (JSGraphicsDevice*) dd->deviceSpecific;
	std::ostringstream os;
	
	os << "{ \"cmd\": \"status\", \"device\": " << pd->device << ", \"data\": { \"msg\": \"closing graphics device\" }}";
	
	pd->write( os.str().c_str() );
	delete pd;
		
}

void draw_line(double x1, double y1, double x2, double y2, const pGEcontext gc, pDevDesc dd) {

	JSGraphicsDevice *pd = (JSGraphicsDevice*) dd->deviceSpecific;
	std::ostringstream os;
	
	os << "{ \"cmd\": \"line\", \"device\": " << pd->device << ", \"data\": "
		<< "{ \"x1\": " << x1 
		<< ", \"y1\": " << y1
		<< ", \"x2\": " << x2
		<< ", \"y2\": " << y2 
		<< ", \"style\": " <<
		write_style_linetype(gc, false)
		<< "}}";

	pd->write(os.str().c_str());

}

void func_poly(int n, double *x, double *y, int filled, const pGEcontext gc, pDevDesc dd) {

	JSGraphicsDevice *pd = (JSGraphicsDevice*) dd->deviceSpecific;
	std::ostringstream os;

	os << "{ \"cmd\": \"poly\", \"device\": " << pd->device << ", \"data\": { \"style\": " 
		<< write_style_linetype(gc, filled)
		<< ", \"points\": [";

	for (int i = 0; i < n; i++) {
		if( i ) os << ", ";
		os << "[" << x[i] << "," << y[i] << "]";
	}

	os << " ] }}";
	
	pd->write(os.str().c_str());
	
}

void draw_polyline(int n, double *x, double *y, const pGEcontext gc, pDevDesc dd) {
	func_poly(n, x, y, 0, gc, dd);
}

void draw_polygon(int n, double *x, double *y, const pGEcontext gc, pDevDesc dd) {
	func_poly(n, x, y, 1, gc, dd);
}

void draw_path(double *x, double *y,
              int npoly, int *nper,
              Rboolean winding,
              const pGEcontext gc, pDevDesc dd) {
	
    cerr << "ENOTIMPL: draw_path (FIXME)" << endl;
    
}

double get_strWidth(const char *str, const pGEcontext gc, pDevDesc dd) {
	JSGraphicsDevice *pd = (JSGraphicsDevice*) dd->deviceSpecific;
	gdtools::context_set_font(pd->cc, fontname(gc->fontfamily, gc->fontface),
		gc->cex * gc->ps, is_bold(gc->fontface), is_italic(gc->fontface));
	FontMetric fm = gdtools::context_extents(pd->cc, std::string(str));
	return fm.width;
}

void draw_rect(double x1, double y1, double x2, double y2,
              const pGEcontext gc, pDevDesc dd) {

	JSGraphicsDevice *pd = (JSGraphicsDevice*) dd->deviceSpecific;
	std::ostringstream os;
	
	os << "{ \"cmd\": \"rect\", \"device\": " << pd->device << ", \"data\": "
		<< "{ \"x1\": " << x1 
		<< ", \"y1\": " << y1
		<< ", \"x2\": " << x2
		<< ", \"y2\": " << y2 
		<< ", \"style\": " <<
		write_style_linetype(gc, true)
		<< "}}";

	pd->write(os.str().c_str());
						  
}

void draw_circle(double x, double y, double r, const pGEcontext gc,
                       pDevDesc dd) {

	JSGraphicsDevice *pd = (JSGraphicsDevice*) dd->deviceSpecific;
	std::ostringstream os;
	
	os << "{ \"cmd\": \"circle\", \"device\": " << pd->device << ", \"data\": "
		<< "{ \"x\": " << x 
		<< ", \"y\": " << y
		<< ", \"r\": " << r
		<< ", \"style\": " <<
		write_style_linetype(gc, true)
		<< "}}";

	pd->write(os.str().c_str());
								  
}


void draw_text(double x, double y, const char *str, double rot,
              double hadj, const pGEcontext gc, pDevDesc dd) {

	JSGraphicsDevice *pd = (JSGraphicsDevice*) dd->deviceSpecific;
	std::ostringstream os;

	static char fontdesc[512];
	sprintf( fontdesc, "%s%s%.2fpx %s", 
		is_italic(gc->fontface) ? "italic " : "",
		is_bold(gc->fontface) ? "bold " : "",
		gc->cex * gc->ps,
		fontname(gc->fontfamily, gc->fontface).c_str()
	);

	// escape the string suitable for utf8 json -- just 
	// quotes, I think (and slashes?)

	int i, len = strlen( str );
	std::string escaped;
	escaped.reserve( len * 1.5 );
	for( i = 0; i< len; i++ ){
		if( str[i] == '"' ) escaped += "\\";
		escaped += str[i];
	}
		
	os << "{\"cmd\": \"text\", \"device\": " << pd->device << ", \"data\":{ \"x\": "
		<< x << ", \"y\": " << y << ", \"rot\": " << rot 
		<< ", \"text\": \"" 
		<< escaped << "\", \"font\": \""
		<< fontdesc << "\", \"fill\": \""
		<< rgba(gc->col) << "\" }} ";

	pd->write(os.str().c_str());
	
}

void get_size( double *left, double *right, double *bottom, double *top, pDevDesc dd) {
	*left = dd->left;
	*right = dd->right;
	*bottom = dd->bottom;
	*top = dd->top;
}

void draw_raster(unsigned int *raster, int w, int h,
                double x, double y,
                double width, double height,
                double rot,
                Rboolean interpolate,
                const pGEcontext gc, pDevDesc dd) {
						 
	JSGraphicsDevice *pd = (JSGraphicsDevice*)dd->deviceSpecific;						 
	std::ostringstream os;
						 
	if (height < 0) height = -height;

	std::vector<unsigned int> raster_(w*h);
	for (std::vector<unsigned int>::size_type i = 0 ; i < raster_.size(); ++i) {
		raster_[i] = raster[i] ;
	}

	std::string base64_str = gdtools::raster_to_str(raster_, w, h, width, height, (Rboolean)interpolate);

	os << "{\"cmd\": \"img\", \"device\": " << pd->device << ", \"data\":{ \"x\": "
		<< x << ", \"y\": " << y 
		<< ", \"rot\": " << rot 
		<< ", \"width\": " << width
		<< ", \"height\": " << height
		<< ", \"dataURL\": \"data:image/png;base64," 
		<< base64_str << "\" }} ";

	pd->write(os.str().c_str());

}

/**
 * constructor
 */
pDevDesc js_device_new( rcolor bg, double width, double height, int pointsize) {

	pDevDesc dd = (DevDesc*) calloc(1, sizeof(DevDesc));
	if (dd == NULL) return dd;

	dd->startfill = bg;
	dd->startcol = R_RGB(0, 0, 0);
	dd->startps = pointsize;
	dd->startlty = 0;
	dd->startfont = 1;
	dd->startgamma = 1;

	dd->activate = NULL;
	dd->deactivate = NULL;
  
	dd->newPage = new_page;
	dd->close = close_device;
	dd->clip = set_clip;
	dd->size = get_size;
	dd->metricInfo = get_metric_info;
	dd->strWidth = get_strWidth;

	dd->line = draw_line;
	dd->text = draw_text;
	dd->rect = draw_rect;
	dd->circle = draw_circle;
	dd->polygon = draw_polygon;
	dd->polyline = draw_polyline;
	dd->path = draw_path;
	dd->raster = draw_raster;
	dd->mode = NULL;
	dd->cap = NULL;

	dd->wantSymbolUTF8 = (Rboolean) 1;
	dd->hasTextUTF8 = (Rboolean) 1;
	dd->textUTF8 = draw_text;
	dd->strWidthUTF8 = get_strWidth;

	dd->left = 0;
	dd->top = 0;
	dd->right = width;
	dd->bottom = height;

	// straight up from [1]
	
	dd->cra[0] = 0.9 * pointsize;
	dd->cra[1] = 1.2 * pointsize;
	dd->xCharOffset = 0.4900;
	dd->yCharOffset = 0.3333;
	dd->yLineBias = 0.2;
	dd->ipr[0] = 1.0 / 72.0;
	dd->ipr[1] = 1.0 / 72.0;

	dd->canClip = FALSE;
	dd->canHAdj = 0;
	dd->canChangeGamma = FALSE;
	dd->displayListOn = TRUE;
	dd->haveTransparency = 2;
	dd->haveTransparentBg = 2;

	dd->deviceSpecific = new JSGraphicsDevice();
	return dd;
}

// [[Rcpp::export]]
SEXP jsclient_callback_sync_( SEXP data, bool buffer = false) 
{
	static CALLBACK_FN_SYNC *callback = (CALLBACK_FN_SYNC*)R_GetCCallable("ControlR", "CallbackSync");
	if( callback ) return callback( data, buffer );
	return R_NilValue;
}

// [[Rcpp::export]]
void jsclient_callback_( std::string channel, SEXP data, bool buffer = false) 
{
	static CALLBACK_FN_SEXP *callback = (CALLBACK_FN_SEXP*)R_GetCCallable("ControlR", "CallbackSEXP");
	if( callback ) callback( channel.c_str(), data, buffer );
}

// [[Rcpp::export]]
void jsclient_device_resize_( int device, int width, int height, bool replay ) 
{
	// FIXME: should check it's indeed our device
	
	pGEDevDesc gd = GEgetDevice( device-1 );
	if( gd ){
			
		gd->dev->right = width;
		gd->dev->bottom = height;
  
		// ... ?
		if( replay ) GEplayDisplayList(gd);
	
	}
	else cerr << "Can't find device " << device << endl;
	
}

// [[Rcpp::export]]
int jsclient_device_( std::string name, std::string background, int width, int height, int pointsize) 
{
	rcolor bg = R_GE_str2col(background.c_str());
	int device = 0;
  
	R_GE_checkVersionOrDie(R_GE_version);
	R_CheckDeviceAvailable();

	BEGIN_SUSPEND_INTERRUPTS {
		pDevDesc dev = js_device_new(bg, width, height, pointsize);
		if (dev == NULL) Rcpp::stop("Failed to create device");
		pGEDevDesc gd = GEcreateDevDesc(dev);
		GEaddDevice2(gd, name.c_str());
		GEinitDisplayList(gd);
		device = GEdeviceNumber(gd) + 1; // to match what R says
		((JSGraphicsDevice*)(dev->deviceSpecific))->setDevice(device);
		// cout << "device number: " << device << endl;
  } END_SUSPEND_INTERRUPTS;

  return device;
  
}

