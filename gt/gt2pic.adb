--------------------------------------------------------------------------
--  RC2GW.adb
--
--  RC2GW translates a Resource Compiler script file (.rc or .dlg)
--  into an Ada package for the GWindows GUI system.
--
--  Copyright (c) Gautier de Montmollin 2008..2012
--  SWITZERLAND
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:

--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.

--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.

-- NB: this is the MIT License, as found 28-Jul-2008 on the site
-- http://www.opensource.org/licenses/mit-license.php
----------------------------------------------------------------------------
--
-- Change log:
--
-- 24-Jun-2009 GdM: Added the -c option (initialize controls, for testing)
--  ~ May-2009 GdM: Added the -t option (test generation)
-- 29-Aug-2008 GdM: Added the -x and -y options
-- 28-Jul-2008 GdM: Created
--

with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Text_IO;                       use Ada.Text_IO;

with GT_IO, GT_Help, YYParse;

procedure GT2Pic is
  Inp_Opened  : Boolean := False;

  procedure Syntax is
  begin
    Put_Line( Standard_Error, "Syntax: gt2pic [option] input_file" );
    New_Line( Standard_Error );
    Put_Line( Standard_Error, "GT2Pic translates a GraphThing file (.gt)" );
    Put_Line( Standard_Error, "into a LaTeX picture, compatible with TeXCAD." );
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
              "GT2Pic: transcripting '" & arg &
              "' to LaTeX picture." );
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
    GT_IO.Close_Input;
  end if;

end GT2Pic;
