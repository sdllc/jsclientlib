#
# Copyright (c) 2016 Structured Data LLC
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#

#' @useDynLib jsClientLib
NA

#------------------------------------------------------------------------------
# data store for watches, file watcher, and progress bars
#------------------------------------------------------------------------------

.data.env <- new.env();

#------------------------------------------------------------------------------
# graphics device and api
#------------------------------------------------------------------------------

#'
#' create a new graphics device
#'
device <- function( name="json-ipc", background = "white", 
	width = 600, height = 400, 	pointsize = 12 ) {
		invisible(.Call('jsClientLib_jsclient_device_', 
			PACKAGE = 'jsClientLib', 
			name, background, width, height, pointsize));
}

#'
#' resize a graphics device
#'
#' @param device A device ID
#' @param width Width in pixels
#' @param height Height in pixels
#' @param replay If True, replay the graphics device history (redraw) 
#'
device.resize <- function( device, width, height, replay=T ){
    invisible(.Call('jsClientLib_jsclient_device_resize_', 
		PACKAGE = 'jsClientLib', device, width, height, replay))
}

#------------------------------------------------------------------------------
# tools
#------------------------------------------------------------------------------

#'
#' get locals
#'
#' the idea is to give a relatively short representation of each field, as
#' a string.  the first line is used as the brief description and the full
#' text is displayed for a selected item.
#'
#' by default we're using \code{print} for functions and \code{str} for all other 
#' fields, and we have wrappers for these that call \code{capture.output}.
#'
#' @param envir The current environment -- generally \code{.GlobalEnv}, but 
#' when debugging, the active function.
#'
# FIXME: this should go into the client, in startup script, to facilitate
# modification for various purposes.  or perhaps we could leave this one
# and allow users to change the call to something more useful.
#
locals <- function( envir ){
	.js.client.callback( "locals", list(
		fields = lapply( mget( ls(envir), envir=envir), function(a){ 
			if( is.function(a)){ string.representation <- capture.print(a); }
			else if( is.environment(a)){ string.representation <- capture.env(a); }
			else { string.representation <- capture.str(a) }
			rslt <- list( value=string.representation, class=class(a));

            if( is.numeric(a) && ( length(a) > 1 )){ rslt$histogram = hist(a, plot=F); }
            rslt;
		}),
		envir = capture.output(str(envir)))
	);
}

#
# watches allow arbitrary expressions; we call eval(), which is dangerous
# but very flexible.  by default the results are presented as str() or 
# print() for functions, but you can pass in a function (or function name)
# for custom results (see, e.g., capture.histogram).
#

assign( "watches", list(), envir=.data.env );

#'
#' Get watches
#'
#' Get watches.  A numbered list is returned, use those indexes to 
#' manipulate watches.
#'
#' @return List of watche variables
#' @seealso \code{\link{js.client.remove.watch}}
#'
watches <- function(){
	.js.client.callback( "watches", list(
		fields = lapply( .data.env$watches, function(a){ 
			envir <- a$envir;
			a$envir <- capture.output(str(envir));
			tryCatch({
				val <- eval( a$expr, envir=envir );
				a$value <- do.call( a$func, list(val));
				a$class <- class(val);
				if( !inherits( a$value, "character" )){ a$value <- capture.output( print( a$value )); }
                if( is.numeric( val ) && (length(val) > 1 )){ a$histogram = hist(val, plot=F); }
				return(a); 
			}, error=function( cond ){ 
				a$err <- toString(cond); 
				return(a);
			});
		})
	));
}

#'
#' Add a watch variable
#'
#' @param expr Any R expression, generally a field, function, or slice
#' @param func The function used to render the field in the viewer.  Defaults
#' to \code{print} for functions and \code{str} for everything else.
#' @param label Optional label, defaults to \code{expr}.
#' @param envir The environment in which to resolve \code{expr}. Defaults 
#' to \code{.GlobalEnv}.
#'
#' @export
js.client.add.watch <- function( expr, func, label, envir=.GlobalEnv ){
	if( missing( func )){
		
		# NOTE: this resolves expr, which will throw an error
		# if expr doesn't exist at this moment.  that might not
		# be desirable.  it's worth considering supporting 
		# non-existent fields or out-of-range indexes.

		# actually this will work if there's a func specified...
		# it only resolves expr if it needs to test whether
		# it's a function or not.
		
		# FIXME: since we allow specification of the function,
		# we should probably skip this and just default to str().
		# users can use print() if they want.
		
		x <- eval( substitute( expr ));
		if( is.function( x )){ func="capture.print"; }
		else { func="capture.str"; }
	}
	else {
		if( !is.character( substitute( func ))){
			func = toString( substitute( func ));
		}
	}
	
	if( missing(label)){
		label = capture.output(print(substitute(expr)));
	}
	
	tmp <- .data.env$watches;
	tmp[[length(tmp)+1]] <- list( label=label, expr=substitute(expr), func=func, envir=envir );
	.data.env$watches <- tmp ;

    # notify the client so it can pop open a view (if desired)
    # .js.client.callback( "add-watch", as.list( environment()));

    .js.client.callback( "add-watch", label );

}

#'
#' Remove a watch variable
#'
#' Remove watch, by index.  watches are somewhat complicated because you
#' can have multiple watches on one field, and we allow a full set of equal
#' parameters; hence there's no reasonable way to look up a value.  
#'
#' @param index the (integer) index into the list of watches
#'
#' @export
js.client.remove.watch <- function( index ){
	.data.env$watches <- .data.env$watches[-index];
}

#'
#' remove all watches
#'
#' @export
js.client.clear.watches <- function(){
	.data.env$watches <- list();
}

# these are utility functions for watch/locals

#'
#' return output of \code{str}
#'
#' calls \code{str} and returns output as a string.  this is a utility 
#' function for use with watches in the js client.
#'
#' @param x Any R object
#' @return the output of \code{str(x)}, as a string
#' @seealso \code{\link{capture.print}}, \code{\link{capture.histogram}}
#'
#' @export
capture.str <- function(x){ capture.output( str(x, give.head=F)); }

#'
#' return output of \code{print}
#'
#' calls \code{print} and returns output as a string.  this is a utility 
#' function for use with watches in the js client.
#'
#' @param x any R object
#' @return the output of \code{print(x)}, as a string
#' @seealso \code{\link{capture.str}}, \code{\link{capture.histogram}}
#'
#' @export
capture.print <- function(x){ capture.output( print(x)); }

#'
#' return output of \code{histogram}
#'
#' calls \code{histogram} and returns output as a string, wihtout plotting
#' the histogram.  this is a utility function for use with watches in the 
#' js client.
#'
#' @param x any R object
#' @return the output of \code{hist(x, plot=F)}, as a string
#' @seealso \code{\link{capture.print}}, \code{\link{capture.str}}
#'
#' @export
capture.histogram <- function(x){ capture.output( print( hist(x, plot=F ))); }

#'
#' return output of \code{ls.str}
#'
#' calls \code{print} and returns output as a string.  this is a utility 
#' function for use with watches in the js client.
#'
#' @param env an environment
#' @return the output of \code{ls.str(x)}, as a string
#' @seealso \code{\link{capture.print}}, \code{\link{capture.str}}, \code{\link{capture.histogram}}
#'
#' @export
capture.env <- function( env ){
	c( capture.output( print( env )), "", "Members:", "", paste0( " ", capture.output( print( ls.str(env)))));
}

#------------------------------------------------------------------------------
# shell utilities
#------------------------------------------------------------------------------

#'
#' Browser function
#'
#' Function for use in \code{options(browser)}
#'
browser <- function( url ){
	.js.client.callback( "browser", as.list( environment()));
}

#'
#' Pager function
#'
#' Function for use in \code{options(pager)}
#'
pager <- function( files, header, title, delete.file ){
	.js.client.callback( "pager", as.list( environment()));
}

#'
#' Quit function
#'
#' Quit function, intended to replace \code{base::quit} and allow the 
#' JS client to exit gracefully.
#'
quit <- function(...){
	.js.client.callback( "system", list( cmd="quit", args=as.list(environment())));
}

#------------------------------------------------------------------------------
# options
#------------------------------------------------------------------------------

.js.client.options.env <- new.env();

#'
#' get and set client.options
#'
#' js.client.options controls options (preferences) in the shell application.
#' it functions like \code{options()}; use list syntax to set values, and 
#' names to get values.  Call with no arguments to list all options.
#'
#' @export
js.client.options <- function(...) {

	args <- list(...);
	if( is.null( names(args))){
		args <- unlist( args );
		if( length( args ) == 0 ){
			args <- unlist( ls( .js.client.options.env ));
		}
		sapply( args, function(a){
			.js.client.options.env[[a]];
		}, simplify=F, USE.NAMES=T );
	}
	else {
		sapply( names( args ), function(n){
			prev <- .js.client.options.env[[n]];
			if( is.null( args[[n]] )){
				if( exists( n, envir=.js.client.options.env )){
					rm( list=c(n), envir=.js.client.options.env );
				}
			}
			else {
				assign( n, args[[n]], envir=.js.client.options.env );
			}
			
			# explicitly pass key and value so we don't lose null values in lists
			.js.client.callback( "preferences", list( KEY=n, VALUE=args[[n]] ));
			prev;
			
		}, simplify=F, USE.NAMES=T );
	}

}

#------------------------------------------------------------------------------
# file watch api
#
# FIXME: use an environment (for storage).  simplifies semantics.
#------------------------------------------------------------------------------

.data.env$file.watches <- list();

#'
#' Watch a file
#'
#' Watch a file and reload (or execute some arbitrary code) when the file changes.
#'
#' @param path Path to the file.  we will call \code{normalizePath}.
#' @param FUNC Code to execute when the file changes.  Defaults to \code{source(path)}.
#' @param override Remove any prior file watches.  You might add two separate watches
#' that execute different functions when a file changes; this is supported, unless the 
#' \code{override} parameter is set. Defaults to True.
#'
#' @export
js.client.watch.file <- function( path, FUNC=NULL, override=T, source.now=F ){
	path = normalizePath(path);
	if( !override ){ unwatch.file( path ); }
	if( source.now ){ source(path); }
	tmp <- .data.env$file.watches;
	tmp[[length(tmp)+1]] <- list( path=path, func=FUNC );
	.data.env$file.watches <- tmp ;
	.js.client.callback( "file.watch", list( command="watch", path=path ));
}

#'
#' Unwatch a file
#'
#' @param path Path to the file
#'
#' @export
js.client.unwatch.file <- function( path ){
	tmp <- .data.env$file.watches;
	for( index in length(tmp):1 ){
		if( tmp[[index]]$path == path ){
			tmp[[index]] <- NULL;
		}
	}
	.data.env$file.watches <- tmp;
	.js.client.callback( "file.watch", list( command="unwatch", path=path ));
}

#'
#' Remove all file watches
#'
#' @export
js.client.unwatch.all <- function(){
	.data.env$file.watches <- list();
	.js.client.callback( "file.watch", list( command="clear" ));
}

#'
#' Notification when a watched file has changed.
#'
#' @param path Path to the file
#'
file.changed <- function( filename, original_path ){
	x<- lapply( .data.env$file.watches, function( watch ){
		if( watch$path == original_path ){
			if( is.null( watch$func )){
				.js.client.callback( "file.watch", list( command="reloading", filename=filename, original_path=original_path ));
				source( filename );
			}
			else {
				.js.client.callback( "file.watch", list( command="executing", filename=filename, original_path=original_path ));
				watch$func(filename);
			}
		}
	});
}

#------------------------------------------------------------------------------
# progress bars (for win/tk replacement, text progress bars are ok as-is)
#------------------------------------------------------------------------------

.data.env$progress.bars <- list();
.data.env$progress.bar.key <- 1;

#'
#' Create progress bar
#'
#' Create a progress bar and return the handle.
#'
#' @param min Minimum value 
#' @param max Maximum value 
#' @param initial Initial value.  Defaults to \code{min} 
#' @param ... Additional arguments (width, style)
#' @return An object of class \code{js.client.progress.bar}
#' @seealso \code{\link{js.client.get.progress.bar}}, \code{\link{js.client.set.progress.bar}}
#'
#' @export
js.client.progress.bar <- function( min=0, max=1, initial=min, ... ){

	key <- .data.env$progress.bar.key;
	struct <- list( key=key, min=min, max=max, initial=initial, value=initial, ... );
	handle <- list( key=key );
	class(handle) <- "js.client.progress.bar";
	.data.env$progress.bars[[toString(key)]] <- struct;
	.js.client.callback( "progress.bar", struct );
	
	# increment key	for next call
	.data.env$progress.bar.key <- .data.env$progress.bar.key + 1;	
	
	return(handle);
}

#'
#' Get progress bar value
#'
#' @param pb An object of class \code{js.client.progress.bar}
#' @seealso \code{\link{js.client.progress.bar}}, \code{\link{js.client.set.progress.bar}}
#'
#' @export
js.client.get.progress.bar <- function( pb ){
	struct <- .data.env$progress.bars[[toString(pb$key)]];
	return( struct$value );	
}

#'
#' Set value in a progress bar
#'
#' @param pb An object of class \code{js.client.progress.bar}
#' @param value A number
#' @seealso \code{\link{js.client.progress.bar}}, \code{\link{js.client.get.progress.bar}}
#'
#' @export
js.client.set.progress.bar <- function( pb, value ){
	struct <- .data.env$progress.bars[[toString(pb$key)]];
	struct$value <- value;
	.data.env$progress.bars[[toString(pb$key)]] <- struct;
	.js.client.callback( "progress.bar", struct );
	return( struct$value );	
}

#'
#' Close a progress bar handle
#'
#' implementation of generic \code{close} for objects of class 
#' \code{js.client.progress.bar}
#'
#' @param pb An object of class \code{js.client.progress.bar}
#' @seealso \code{\link{js.client.progress.bar}}, \code{\link{js.client.get.progress.bar}},
#' \code{\link{js.client.set.progress.bar}}
#'
#' @export
close.js.client.progress.bar <- function( pb ){
	struct <- .data.env$progress.bars[[toString(pb$key)]];
	struct$closed <- T;
	.js.client.callback( "progress.bar", struct );
	.data.env$progress.bars[[toString(pb$key)]] <- NULL;
}

#------------------------------------------------------------------------------
# generic
#------------------------------------------------------------------------------

.js.client.callback <- function( channel, data, buffer = FALSE ){
    invisible(.Call('jsClientLib_jsclient_callback_', 
		PACKAGE = 'jsClientLib', channel, data, buffer))
}

.js.client.callback.sync <- function(data, buffer = FALSE) {
    .Call('jsClientLib_jsclient_callback_sync_', PACKAGE = 'jsClientLib', data, buffer)
}
