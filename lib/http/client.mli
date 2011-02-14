(*
  OCaml HTTP - do it yourself (fully OCaml) HTTP daemon

  Copyright (C) <2002-2005> Stefano Zacchiroli <zack@cs.unibo.it>
  Copyright (C) <2009> David Sheets <sheets@alum.mit.edu>

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU Library General Public License as
  published by the Free Software Foundation, version 2.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
  USA
*)

(** Minimal implementation of an HTTP 1.0/1.1 client. Interface is
    similar to Gerd Stoplmann's Http_client module. Implementation is
    simpler and doesn't handle HTTP redirection, proxies, ecc. The
    only reason for the existence of this module is for performances
    and incremental elaboration of response's bodies.  All the HTTP
    methods are support with to flavors: one in which the response
    body is returned as a string alongside the response headers, and
    another in which the response body is written to a output channel.
    The latter is intended to support payloads whose size exceeds
    available memory.  *)

type headers = (string * string) list

type response_body = OS.Istring.View.t Lwt_sequence.t Lwt.t
exception Http_error of int * headers * response_body

(**
   @param headers optional overriding headers
   @param url an HTTP url
   @return HTTP GET response's body
   @raise Http_error when response code <> 200 
*)
val get : Net.Flow.TCPv4.mgr -> ?src:Net.Flow.TCPv4.src -> ?headers:headers -> string -> (headers * response_body) Lwt.t

(**
   @param headers optional overriding headers
   @param url an HTTP url
   @return HTTP HEAD raw response
   @raise Http_error when response code <> 200 
*)
val head : Net.Flow.TCPv4.mgr -> ?src:Net.Flow.TCPv4.src -> ?headers:headers -> string -> (headers * response_body) Lwt.t

(**
   @param headers optional overriding headers
   @param body optional message
   @param url an HTTP url
   @return HTTP POST raw response
   @raise Http_error when response code <> 200 
*)
val post : Net.Flow.TCPv4.mgr -> ?src:Net.Flow.TCPv4.src -> ?headers:headers -> ?body:string -> string -> (headers * response_body) Lwt.t

(**
   @param headers optional overriding headers
   @param body optional message, whose source is either a string or an input channel
   @param url an HTTP url
   @return HTTP PUT raw response
   @raise Http_error when response code <> 200 
*)
val put : Net.Flow.TCPv4.mgr -> ?src:Net.Flow.TCPv4.src -> ?headers:headers -> ?body:string -> string -> (headers * response_body) Lwt.t

(**
   @param headers optional overriding headers
   @param url an HTTP url
   @return HTTP DELETE raw response
   @raise Http_error when response code <> 200 
*)
val delete : Net.Flow.TCPv4.mgr -> ?src:Net.Flow.TCPv4.src -> ?headers:headers -> string -> (headers * response_body) Lwt.t