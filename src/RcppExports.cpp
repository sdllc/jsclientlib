// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// jsclient_callback_sync_
SEXP jsclient_callback_sync_(SEXP data, bool buffer);
RcppExport SEXP jsClientLib_jsclient_callback_sync_(SEXP dataSEXP, SEXP bufferSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type data(dataSEXP);
    Rcpp::traits::input_parameter< bool >::type buffer(bufferSEXP);
    __result = Rcpp::wrap(jsclient_callback_sync_(data, buffer));
    return __result;
END_RCPP
}
// jsclient_callback_
void jsclient_callback_(std::string channel, SEXP data, bool buffer);
RcppExport SEXP jsClientLib_jsclient_callback_(SEXP channelSEXP, SEXP dataSEXP, SEXP bufferSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::string >::type channel(channelSEXP);
    Rcpp::traits::input_parameter< SEXP >::type data(dataSEXP);
    Rcpp::traits::input_parameter< bool >::type buffer(bufferSEXP);
    jsclient_callback_(channel, data, buffer);
    return R_NilValue;
END_RCPP
}
// jsclient_device_resize_
void jsclient_device_resize_(int device, int width, int height, bool replay);
RcppExport SEXP jsClientLib_jsclient_device_resize_(SEXP deviceSEXP, SEXP widthSEXP, SEXP heightSEXP, SEXP replaySEXP) {
BEGIN_RCPP
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< int >::type device(deviceSEXP);
    Rcpp::traits::input_parameter< int >::type width(widthSEXP);
    Rcpp::traits::input_parameter< int >::type height(heightSEXP);
    Rcpp::traits::input_parameter< bool >::type replay(replaySEXP);
    jsclient_device_resize_(device, width, height, replay);
    return R_NilValue;
END_RCPP
}
// jsclient_device_
int jsclient_device_(std::string name, std::string background, int width, int height, int pointsize);
RcppExport SEXP jsClientLib_jsclient_device_(SEXP nameSEXP, SEXP backgroundSEXP, SEXP widthSEXP, SEXP heightSEXP, SEXP pointsizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::string >::type name(nameSEXP);
    Rcpp::traits::input_parameter< std::string >::type background(backgroundSEXP);
    Rcpp::traits::input_parameter< int >::type width(widthSEXP);
    Rcpp::traits::input_parameter< int >::type height(heightSEXP);
    Rcpp::traits::input_parameter< int >::type pointsize(pointsizeSEXP);
    __result = Rcpp::wrap(jsclient_device_(name, background, width, height, pointsize));
    return __result;
END_RCPP
}
