:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).

% http_reply_from_files is in here
:- use_module(library(http/http_files)).

:- multifile http:location/3.
:- dynamic   http:location/3.
http:location(files, '/f', []).
:- http_handler(files(.), http_reply_from_files(assets, [cache(true)]), [prefix]).
:- http_handler(/, http_reply_file('assets/index.html', []), []).

:- use_module(library(pengines)).
:- use_module(pengine_sandbox:myfamily).
:- use_module(myfamily).

% :- multifile sandbox:safe_primitive/1.
%
% sandbox:safe_primitive(myfamily:male(_)).
% sandbox:safe_primitive(myfamily:female(_)).

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

:- initialization server(3000).
