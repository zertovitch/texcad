with Ada.Text_IO;                       use Ada.Text_IO;

-- Project specific:
with GT_Help;

procedure GT_Error (s: in String) is
begin
  -- close files
  --
  Put_Line(Current_Error,s);
  raise GT_Help.syntax_error;
end GT_Error;
