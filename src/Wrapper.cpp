
//
// replacement for Rcpp exports file (Rcpp was removed to minimize deps)
//
// TODO: raise errors where appropriate
// TODO: validate parameters 
//

#include <string>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP jsclient_callback_sync_(SEXP data, bool buffer);
SEXP jsClientLib_jsclient_callback_sync_(SEXP dataSEXP, SEXP bufferSEXP) {
    return jsclient_callback_sync_( dataSEXP, Rf_asLogical( bufferSEXP ));
}

void jsclient_callback_(std::string channel, SEXP data, bool buffer);
SEXP jsClientLib_jsclient_callback_(SEXP channelSEXP, SEXP dataSEXP, SEXP bufferSEXP) {
    const char *channel = CHAR(STRING_ELT(channelSEXP, 0));
    jsclient_callback_( channel, dataSEXP, Rf_asLogical( bufferSEXP ));
    return R_NilValue;
}

void jsclient_device_resize_(int device, int width, int height, bool replay);
SEXP jsClientLib_jsclient_device_resize_(SEXP deviceSEXP, SEXP widthSEXP, SEXP heightSEXP, SEXP replaySEXP) {
    jsclient_device_resize_( Rf_asInteger( deviceSEXP ), Rf_asInteger( widthSEXP ), Rf_asInteger( heightSEXP ), Rf_asInteger( replaySEXP ));
    return R_NilValue;
}

int jsclient_device_(std::string name, std::string background, int width, int height, int pointsize);
SEXP jsClientLib_jsclient_device_(SEXP nameSEXP, SEXP backgroundSEXP, SEXP widthSEXP, SEXP heightSEXP, SEXP pointsizeSEXP) {
    const char *name = CHAR(STRING_ELT(nameSEXP, 0));
    const char *background = CHAR(STRING_ELT(backgroundSEXP, 0));
    int rslt = jsclient_device_( name, background, Rf_asInteger( widthSEXP ), Rf_asInteger( heightSEXP ), Rf_asInteger( pointsizeSEXP ));
    return Rf_ScalarReal(rslt);
}

extern "C" {

    void R_init_jsClientLib(DllInfo *info)
    {
        static R_CallMethodDef callMethods[]  = {
            { "jsClientLib_jsclient_callback_sync_", (DL_FUNC)&jsClientLib_jsclient_callback_sync_, 2},
            { "jsClientLib_jsclient_callback_", (DL_FUNC)&jsClientLib_jsclient_callback_, 3},
            { "jsClientLib_jsclient_device_resize_", (DL_FUNC)&jsClientLib_jsclient_device_resize_, 4},
            { "jsClientLib_jsclient_device_", (DL_FUNC)&jsClientLib_jsclient_device_, 5},
            { NULL, NULL, 0 }
        };
        R_registerRoutines( info, NULL, callMethods, NULL, NULL);
    }

}
