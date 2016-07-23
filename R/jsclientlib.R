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
# async and (semi)sync calls back to the client
#------------------------------------------------------------------------------

.js.client.callback <- function( channel, data, buffer = FALSE ){
    invisible(.Call('jsClientLib_jsclient_callback_', 
		PACKAGE = 'jsClientLib', channel, data, buffer))
}

.js.client.callback.sync <- function(data, buffer = FALSE) {
    .Call('jsClientLib_jsclient_callback_sync_', PACKAGE = 'jsClientLib', data, buffer)
}

.client.env <- new.env();
