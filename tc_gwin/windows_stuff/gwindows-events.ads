------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                       G W I N D O W S . E V E N T S                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2005 David Botton                   --
--                                                                          --
-- This is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. It is distributed in the hope that it will be useful,  but WITHOUT --
-- ANY WARRANTY;  without  even the  implied warranty of MERCHANTABILITY or --
-- FITNESS FOR A PARTICULAR PURPOSE.    See the GNU General  Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with this;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- More information about GWindows and the latest current release can       --
-- be located on the web at http://www.gnavi.org/gwindows                   --
--                                                                          --
------------------------------------------------------------------------------

with GWindows.Base;

package GWindows.Events is

   procedure Do_Window_Close
     (Window : in out GWindows.Base.Base_Window_Type'Class);
   --  Closes window

   procedure Do_End_Application
     (Window : in out GWindows.Base.Base_Window_Type'Class);
   --  Calls GWindows.Base.End_Application

   procedure Do_End_Loop
     (Window : in out GWindows.Base.Base_Window_Type'Class);
   --  Calls GWindows.Base.End_Loop

   procedure Do_Dialog_Cancel
     (Window : in out GWindows.Base.Base_Window_Type'Class);
   --  Ends dialog with IDCANCEL

   procedure Do_Dialog_OK
     (Window : in out GWindows.Base.Base_Window_Type'Class);
   --  Ends dialog with IDOK

end GWindows.Events;
