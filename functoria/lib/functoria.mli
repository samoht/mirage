(*
 * Copyright (c) 2015 Gabriel Radanne <drupyog@zoho.com>
 * Copyright (c) 2015-2020 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** The Functoria DSL.

    The Functoria DSL allows users to describe how to create portable and
    flexible applications. It allows to pass application parameters easily using
    command-line arguments either at configure-time or at runtime.

    Users of the Functoria DSL composes their application by defining a list of
    {{!foreign} module} implementations, specify the command-line {!keys} that
    are required and {{!combinators} combine} all of them together using
    {{:http://dx.doi.org/10.1017/S0956796807006326} applicative} operators.

    The DSL expression is then compiled into an {{!app} application builder},
    which will, once evaluated, produced the final portable and flexible
    application. *)

module Type = Functoria_type
module Impl = Functoria_impl
module Info = Functoria_info
module Device = Functoria_device
module Key = Functoria_key
module Package = Functoria_package
module Engine = Functoria_engine
module Cli = Functoria_cli
