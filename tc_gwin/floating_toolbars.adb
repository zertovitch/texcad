--  GdM 17-Sep-2009: Toolbar windows bug: crash on close, due to a
--                   double recursion. But didn't help.
--                   Solution, a design change: window is created once
--                   and then shown/hidden on status change.
--
--  Created 6-Jan-2004

with GWindows.GStrings;

--  !! For test: with GWindows.Message_Boxes;            use GWindows.Message_Boxes;

package body Floating_Toolbars is

   function Correct_bar_width (tb : Floating_Toolbar) return Integer is
     w : Integer;
   begin
     --  Windows' estimation for Toolbar's width can be...
     w := Integer'Max (tb.bar.mw, Client_Area_Width (tb.bar));   -- ...too narrow...
      --     if tb.bar.w > tb.bar.mw then
      --       w:= Integer'Min(w,tb.bar.w);  -- ...or too wide.
      --     end if;
     return w;
   end Correct_bar_width;

   function Correct_bar_height (tb : Floating_Toolbar) return Integer is
     h : Integer;
   begin
     --  Windows' estimation for Toolbar's width can be...
     h := Integer'Min (tb.bar.mh, Client_Area_Height (tb.bar));   -- ...too high
     if Client_Area_Width (tb.bar) = tb.bar.mw then
       h := tb.bar.mh;
     end if;
     return h;
   end Correct_bar_height;

  procedure Memorize_dimensions (tb : in out Floating_Toolbar) is
  begin
    tb.window.geom.l := Left (tb.window);
    tb.window.geom.t := Top (tb.window);
    tb.window.geom.w := Client_Area_Width (tb.window);
    tb.window.geom.h := Client_Area_Height (tb.window);
    tb.bar.w := Correct_bar_width (tb);
    tb.bar.h := Client_Area_Height (tb.bar);
  end Memorize_dimensions;

  procedure Best_Size (Window : in out Floating_Window) is
  begin
    Client_Area_Size
      (Window,
       Correct_bar_width (Window.belongs_to.all),
       Correct_bar_height (Window.belongs_to.all));
    Memorize_dimensions (Window.belongs_to.all);
  end Best_Size;

  -----------------------
  -- Overriden methods --
  -----------------------

  overriding procedure On_Size (Window : in out Floating_Window;
                                Width  : in     Integer;
                                Height : in     Integer)
  is
    use GWindows.Windows;
  begin
    --  Freeze(Window);
    On_Size (Window_Type (Window), Width, Height); -- Ancestor method
    Best_Size (Window);
    Dock_Children (Window);
    --  Thaw(Window);
    Redraw (Window);
  end On_Size;

  overriding procedure On_Move (Window : in out Floating_Window;
                                Left  : in     Integer;
                                Top   : in     Integer)
  is
    use GWindows.Windows;
  begin
    On_Move (Window_Type (Window), Left => Left, Top => Top);
    --  GW bug until 5-Jan-2012, gnavi rev. 109: On_Move inverted Left & Top !!
    Memorize_dimensions (Window.belongs_to.all);
  end On_Move;

  overriding procedure On_Close (Window    : in out Floating_Window;
                                 Can_Close :    out Boolean)
  is
  begin
    if Window.belongs_to.status = invisible then
      null;
      --  Change_status is calling Close.
      --  It's bad to call Change_status from there !
    else
      Change_status (Window.belongs_to.all, invisible);
    end if;
    Can_Close := False;
    --  Never close!
    --  We just hide: closing cause crashes for mysterious reasons
    --  on certain GNAT versions, but because of one of our bugs...
    --  In addition, it permits to create the window once for all.
  end On_Close;

  overriding procedure On_Button_Select
    (Control : in out GUI_toolbar;
     Item    : in     Integer)
  is
    use GWindows.Windows;
  begin
    if Control.belongs_to /= null then
      On_Menu_Select (Window_Type (Control.belongs_to.parent.all), Item);
    end if;
  end On_Button_Select;

  -------------------
  -- Change_status --
  -------------------

  procedure Change_status (tb : in out Floating_Toolbar; to : Floating_TB_Status)
  is
    from : constant Floating_TB_Status := tb.status;
  begin
    --   Message_Box("",
    --     "From " &  Floating_TB_status'Image(from) & ASCII.CR &
    --     "To   " &  Floating_TB_status'Image(to)
    --   );

    --
    --  1/ Bring to 'invisible' status
    --

    tb.status := invisible;
    if from = windowed then
      Memorize_dimensions (tb);
      --  Message_Box("", "Call window hide");
      tb.window.Hide;
    end if;

    --
    --  2/ Bring from 'invisible' to new status
    --

    case to is
      when invisible => null;  --  Already done
      when windowed  => tb.window.Show;
    end case;

    tb.status := to;
    if from /= to and then tb.Notify_change /= null then
      tb.Notify_change.all (tb);
    end if;
  end Change_status;

  procedure Rotate_status (tb : in out Floating_Toolbar) is
    ns : Floating_TB_Status := tb.status;
  begin
    if ns = Floating_TB_Status'Last then
      ns := Floating_TB_Status'First;
    else
      ns := Floating_TB_Status'Succ (ns);
    end if;
    Change_status (tb, ns);
  end Rotate_status;

  procedure Neutral_resize (Window : in out Floating_Window) is
  --  Forces a smart resizing
  begin
    Size (Window, Width (Window), Height (Window));
  end Neutral_resize;

  ------------
  -- Create --
  ------------

  procedure Create
   (Control    : in out Floating_Toolbar;
    Parent     : in out GWindows.Base.Base_Window_Type'Class;
    Title      : in     GWindows.GString;
    Left       : in     Integer;
    Top        : in     Integer;
    Width      : in     Integer;
    Min_Width  : in     Integer;
    Height     : in     Integer;
    Max_Height : in     Integer;
    Notify     : in     Notify_status_changed_proc)
  is
    memo : constant Floating_TB_Status := Control.status;

    procedure Create_bar (GUI_parent : in out GWindows.Base.Base_Window_Type'Class) is
    begin
      Create
        (Control.bar,
         GUI_parent,
         GWindows.Constants.Use_Default,
         GWindows.Constants.Use_Default,
         Control.bar.w, Control.bar.h,
         Is_Dynamic => True);
      Control.bar.belongs_to := Control'Unrestricted_Access;
    end Create_bar;

    procedure Create_tool_window is
      memo_geom : LTWH_Rectangle;
      use GWindows.Base;
    begin
      --  a/ The tool window
      Control.window.belongs_to := Control'Unrestricted_Access;
      memo_geom := Control.window.geom;

      Create_As_Tool_Window
        (Control.window, Control.parent.all,
         GWindows.GStrings.To_GString_From_Unbounded (Control.title),
         Width  => Control.window.geom.w,
         Height => Control.window.geom.h,
         Is_Dynamic => True);

      --  b/ the tool bar in the tool window
      Create_bar (Control.window);

      --  For the GUI, the relation is bar -> window -> owner

      Dock_Children (Control.window);

      if memo_geom.l /= GWindows.Constants.Use_Default then
        --  Restore previous position:
        GWindows.Base.Left (Base_Window_Type (Control.window), memo_geom.l);
        GWindows.Base.Top (Base_Window_Type (Control.window), memo_geom.t);
      end if;

      if memo_geom.w /= GWindows.Constants.Use_Default then
        --  Restore previous position:
        GWindows.Base.Width (Base_Window_Type (Control.window), memo_geom.w);
        GWindows.Base.Height (Base_Window_Type (Control.window), memo_geom.h);
      end if;

      Redraw (Control.window);
      --  ^ Sometimes needed (Windows first displays half of buttons!)
      Show (Control.bar);  --  We show the bar, but not the containing window!
    end Create_tool_window;

    use GWindows.Base;

  begin
    Control.parent := Window_From_Handle (Handle (Parent));
    Control.title := GWindows.GStrings.To_GString_Unbounded (Title);
    --
    Control.window.geom := (Left, Top, Width, Height);
    --
    Control.bar.w := Width;
    Control.bar.h := Height;
    Control.bar.mw := Min_Width;
    Control.bar.mh := Max_Height;
    Control.Notify_change := Notify;
    --
    Create_tool_window;
    Neutral_resize (Control.window);
    --
    Control.status := invisible;
    Change_status (Control, memo);
  end Create;

end Floating_Toolbars;
