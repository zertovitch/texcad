with GT_Tokens;

package GT_YYroutines is

  use GT_Tokens;

  function YYLex return Token;

  procedure UnYYLex(tok : Token);

end;
