with GT_YYlex, GT_Tokens;

package body GT_YYroutines is
  use GT_Tokens;

  Lookahead : Token;
  HaveLookahead : Boolean := False;
  SecondUnYYLex : exception;

  function YYLex return Token is
  begin
    if HaveLookahead then
      HaveLookahead := False;
      return Lookahead;
    else
      return GT_YYlex;
    end if;
  end YYLex;

  procedure UnYYLex(tok : Token) is
  begin
    if HaveLookahead then
      raise SecondUnYYLex;
    else
      HaveLookahead := True;
      Lookahead := tok;
    end if;
  end UnYYLex;

end;
