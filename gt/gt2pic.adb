--------------------------------------------------------------------------
--  gt2pic.adb
--------------------------------------------------------------------------
--
-- Change log:
--

with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Text_IO;                       use Ada.Text_IO;

with TC;
with GT_IO, GT_Help, YYParse;

procedure GT2Pic is
  Inp_Opened  : Boolean := False;

  procedure Syntax is
  begin
    Put_Line( Standard_Error, "Syntax: gt2pic [option] input_file" );
    New_Line( Standard_Error );
    Put_Line( Standard_Error, "GT2Pic translates a GraphThing file (.gt)" );
    Put_Line( Standard_Error, "into a LaTeX picture, compatible with TeXCAD." );
    Put_Line( Standard_Error, "Build: " & TC.Reference);
    Put_Line( Standard_Error, "Web: " & TC.Web);
    New_Line( Standard_Error );
    Put_Line( Standard_Error, "options:  (none at the moment!)");
  end Syntax;

begin
  GT_Help.Init;

  for i in 1..Argument_Count loop
    declare
      arg: constant String:= Argument(i);
      u_arg: constant String:= To_Upper( arg );
    begin
      if u_arg'length > 1 and then
        (u_arg(1) = '-' or u_arg(1) = '/') then
        case u_arg(2) is
          when others =>  -- includes "-h", "/?" etc.
            Syntax;
            return;
        end case;
      else -- no option
        if Inp_Opened then          -- Two inputs ?!
          GT_IO.Close_Input;
          Syntax;
          return;
        else
          -- RC_Help.source_name:= RC_Help.U(arg);
          begin
            GT_IO.Open_Input (fname => arg);
            Inp_Opened := True;
            Put_Line(Standard_error,
              "GT2Pic: exporting '" & arg & "' to a LaTeX picture." );
          exception
            when Name_Error =>
              Put_Line( Standard_Error, "Input file '" & arg &
                "' not found." );
              Syntax;
              return;
          end;
        end if;
      end if;
    end;
  end loop;

  if not Inp_opened then
    Put_Line(Standard_error,"Missing input file!");
    Syntax;
    return;
  end if;

  --  RC_Help.has_input:= inp_opened;

  --  RC_Help.Ada_Begin;

  YYParse;

  if Inp_Opened then
    GT_Help.Save_picture(Name(GT_IO.user_input_file) & ".pic");
    GT_IO.Close_Input;
  end if;


end GT2Pic;
