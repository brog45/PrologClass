% capturing input
%
beep_boop --> 
    anything, beep(Suffix),
    {
        format('Suffix="~s"~n', [Suffix])        
    }, 
    anything, boop(Suffix), 
    anything. %, at_end.

beep(X) -->
    "beep",
    suffix(X).

boop(X) -->
    "boop",
    suffix(X).

suffix([H|T]) -->
      [H],  % The magic - we grab the digit here
      {
          code_type(H, digit), !
      },
      suffix(T).
suffix([]) --> []. % must be 2nd suffix clause, or the digits wind up in anything

at_end --> \+ [_].

% At bottom for efficiency. At the top, would match beep first
anything --> [].
anything --> [_], anything.
% A subtlety here.  "foo 7 beep1 bar boop14 fdds" is part of the language
