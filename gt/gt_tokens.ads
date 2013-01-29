package Gt_Tokens is



  type const_type is (
    intval,
    floatval,
    doubleval,
    stringval,
    any_type
  );

  type YYSType is record
     text    : String(1..80);
     length  : Natural := 0;
     vartype : const_type;
     intval  : Integer;
     floatval: Long_Float;
  end record;


    YYLVal, YYVal : YYSType; 
    type Token is
        (End_Of_Input, Error, Number, Qstring,
         Tag, Lbrace_T, Rbrace_T,
         Equal_T, Lparen_T, Rparen_T,
         Comma_T, Kwinfo, Kwdescription,
         Kwvertex, Kwedge, Kwat,
         Kwwith, Kwweight, Kwdash,
         Kwleft, Kwright );

    Syntax_Error : exception;

end Gt_Tokens;
