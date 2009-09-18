-- Created 6-Jan-2004

with GWindows.GStrings;

-- !! For test: with GWindows.Message_Boxes;            use GWindows.Message_Boxes;

package body Floating_toolbars is

   function Correct_bar_width(tb: Floating_toolbar) return Integer is
     w: Integer;
   begin
     -- Windows' estimation for Toolbar's width can be...
     w:= Integer'Max(tb.bar.mw,Width(tb.bar));   -- ...too narrow...
      --     if tb.bar.w > tb.bar.mw then
      --       w:= Integer'Min(w,tb.bar.w);  -- ...or too wide.
      --     end if;
     return w;
   end Correct_bar_width;

   function Correct_bar_height(tb: Floating_toolbar) return Integer is
     h: Integer;
   begin
     -- Windows' estimation for Toolbar's width can be...
     h:= Integer'Min(tb.bar.mh,Height(tb.bar));   -- ...too high
     return h;
   end Correct_bar_height;

  procedure Memorize_dimensions(tb: in out Floating_toolbar) is
  begin
    tb.window.geom.l:= Left(tb.window);
    tb.window.geom.t:= Top(tb.window);
    tb.window.geom.w:= Width(tb.window);
    tb.window.geom.h:= Height(tb.window);
    tb.bar.w:= Correct_bar_width(tb);
    tb.bar.h:= Height(tb.bar);
  end Memorize_dimensions;

  procedure Best_Size (Window : in out Floating_window) is
  begin
    Client_Area_Size(Window,
      Correct_bar_width(Window.belongs_to.all),
      Correct_bar_height(Window.belongs_to.all)
    );
    Memorize_dimensions(Window.belongs_to.all);
  end Best_Size;

  procedure On_Size (Window : in out Floating_window;
                     Width  : in     Integer;
                     Height : in     Integer) is
    use GWindows.Windows;
  begin
    -- Freeze(Window);
    On_Size(Window_type(Window),Width,Height); -- Ancestor method
    Best_Size(Window);
    Dock_Children(Window);
    -- Thaw(Window);
    Redraw(Window);
  end On_Size;

  procedure On_Move (Window : in out Floating_window;
                     Top    : in     Integer;
                     Left   : in     Integer) is
    use GWindows.Windows;
  begin
    On_Move( Window_type(Window), Left=> Left, Top=> Top );
    -- GW bug: On_Move inverts Left & Top !!
    Memorize_dimensions(Window.belongs_to.all);
  end On_Move;

  procedure On_Button_Select (
        Control : in out GUI_toolbar;
        Item    : in     Integer           ) is
    use GWindows.Windows;
  begin
    if Control.belongs_to /= null then
      On_Menu_Select (Window_Type(Control.belongs_to.parent.all), Item);
    end if;
  end On_Button_Select;

  procedure Do_Close
     (Window    : in out GWindows.Base.Base_Window_Type'Class;
      Can_Close :    out Boolean);
   --  Switch tool bar back to main window on floating tool bar close

  -------------------
  -- Change_status --
  -------------------

  procedure Change_status (tb: in out Floating_toolbar; to: Floating_TB_status) is
    use GWindows.Base, GWindows.Common_Controls;
    memo_stat: constant Floating_TB_status:= tb.status;
    memo_geom: LTWH_Rectangle;

    procedure Recreate_bar(GUI_parent: in out GWindows.Base.Base_Window_Type'Class) is
    begin
      if tb.Reset_bar /= null then
        Create(
          Toolbar_Control_Type(tb.bar),
          GUI_parent,
          GWindows.Constants.Use_Default,
          GWindows.Constants.Use_Default,
          tb.bar.w, tb.bar.h,
          Is_Dynamic => True
        );
        Visible(tb.bar, False);
        tb.bar.belongs_to:= tb'Unrestricted_Access;
        tb.Reset_bar.all(tb);
      end if;
    end Recreate_bar;

  begin
    -- 1/ Bring to 'invisible' status

    if tb.status /= invisible then
      Close(tb.bar);
    end if;
    tb.status:= invisible;
    -- ^ Windows bug: on Win 2K (not XP) the window is closed
    -- infinitely despite new parent of toolbar.
    case memo_stat is
      when invisible => null;
      when windowed  => -- Store size and position for later
        Memorize_dimensions(tb);
        Close(tb.window);
    end case;

    -- 2/ Bring from 'invisible' to new status

    case to is
      when invisible => null; -- already done
      when windowed  =>
        -- a/ The window
        tb.window.belongs_to:= tb'Unrestricted_Access;
        memo_geom:= tb.window.geom;

        Create_As_Tool_Window
          (tb.window, tb.parent.all,
           GWindows.GStrings.To_GString_From_Unbounded(tb.title),
           Width  => tb.window.geom.w,
           Height => tb.window.geom.h,
           Is_Dynamic => True);

        -- b/ the bar
        Recreate_bar(tb.window);

        --        -- Minimize unused space:
        --        Best_Size(tb.window);

        -- For the GUI, the relation is bar -> window -> owner

        Dock_Children(tb.window);

        if memo_geom.l /= GWindows.Constants.Use_Default then
          -- Restore previous position:
          Left( tb.window, memo_geom.l);
          Top( tb.window, memo_geom.t);
        end if;

        if memo_geom.w /= GWindows.Constants.Use_Default then
          -- Restore previous position:
          Width( tb.window, memo_geom.w);
          Height( tb.window, memo_geom.h);
        end if;

        On_Close_Handler (tb.window, Do_Close'Unrestricted_Access);

        Redraw(tb.window);
        -- ^ Sometimes needed (Windows first displays half of buttons!)

        Visible(tb.window);
        Visible(tb.bar);

    end case;
    tb.status:= to;
    if memo_stat /= to and then tb.Notify_change /= null then
      tb.Notify_change.all(tb);
    end if;
  end Change_status;

  procedure Rotate_status(tb: in out Floating_toolbar) is
    ns: Floating_TB_status:= tb.status;
  begin
    if ns = Floating_TB_status'Last then
      ns:= Floating_TB_status'First;
    else
      ns:= Floating_TB_status'Succ(ns);
    end if;
    Change_status(tb,ns);
  end Rotate_status;

   procedure Do_Close
     (Window    : in out GWindows.Base.Base_Window_Type'Class;
      Can_Close :    out Boolean)
   is
   begin
     if Window in Floating_window'Class then
       Change_status(Floating_window(Window).belongs_to.all, invisible);
       Can_Close := True;
     end if;
   end Do_Close;

  ------------
  -- Create --
  ------------

  procedure Create
   (Control    : in out Floating_toolbar;
    Parent     : in out GWindows.Base.Base_Window_Type'Class;
    Title      : in     GWindows.GString;
    Left       : in     Integer;
    Top        : in     Integer;
    Width      : in     Integer;
    Min_Width  : in     Integer;
    Height     : in     Integer;
    Max_Height : in     Integer;
    Reset      : in     Bar_reset_proc;
    Notify     : in     Notify_status_changed_proc)
  is
    use GWindows.Base;
    memo: constant Floating_TB_status:= Control.status;
  begin
    Control.parent:= Window_From_Handle(Handle(Parent));
    Control.title:= GWindows.GStrings.To_GString_Unbounded( Title );
    --
    Control.window.geom:= (Left,Top,Width,Height);
    --
    Control.bar.w:= Width;
    Control.bar.h:= Height;
    Control.bar.mw:= Min_Width;
    Control.bar.mh:= Max_Height;
    Control.Reset_bar:= Reset;
    Control.Notify_change:= Notify;

    Control.status:= invisible;
    Change_status(Control, memo);
  end Create;

end Floating_toolbars;

