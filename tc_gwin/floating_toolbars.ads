with
  GWindows.Base,
  GWindows.Common_Controls,
  GWindows.Constants,
  GWindows.Image_Lists,
  GWindows.Windows;

package Floating_Toolbars is

  type Floating_TB_Status is (invisible, windowed);

  type Floating_Toolbar;
  type p_Floating_Toolbar is access all Floating_Toolbar;

  -- ***** Floating_window: child of Floating_toolbar, GUI parent of the GUI_Toolbar

  type LTWH_Rectangle is record
    l,t,w,h : Integer;
  end record;

  type Floating_Window is
    new GWindows.Windows.Window_Type with
  record
    geom      : LTWH_Rectangle:= (others=> GWindows.Constants.Use_Default);
    belongs_to: p_Floating_Toolbar;
  end record;

  -- Overriden methods

  procedure On_Size (Window : in out Floating_Window;
                     Width  : in     Integer;
                     Height : in     Integer);

  procedure On_Move (Window : in out Floating_Window;
                     Left   : in     Integer;
                     Top    : in     Integer);

  procedure On_Close (Window    : in out Floating_Window;
                      Can_Close :    out Boolean);

  -- ***** GUI_toolbar: the "bare-bones" toolbar, in the GUI sense

  type GUI_toolbar is
    new GWindows.Common_Controls.Toolbar_Control_Type with
  record
    w,mw,h,mh    : Integer:= GWindows.Constants.Use_Default;
    belongs_to   : p_Floating_Toolbar;
    string_count : Natural:= 0;
  end record;

  procedure On_Button_Select (Control : in out GUI_toolbar;
                              Item    : in     Integer);

  -- ***** Floating_toolbar: the main structure

  type Notify_status_changed_proc is access procedure(tb: in Floating_Toolbar);

  type Floating_toolbar is record
    -- tagged limited
    bar          : GUI_toolbar;     -- the visible bar (if any)
    window       : Floating_Window; -- the visible window (if any)
    images       : GWindows.Image_Lists.Image_List_Type;
    parent       : GWindows.Base.Pointer_To_Base_Window_Class;
    title        : GWindows.GString_Unbounded;
    status       : Floating_TB_Status:= invisible;
    Notify_change: Notify_status_changed_proc:= null;
  end record;

  procedure Change_status(tb: in out Floating_Toolbar; to: Floating_TB_Status);

  procedure Rotate_status(tb: in out Floating_Toolbar);

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
    Notify     : in     Notify_status_changed_proc);

end Floating_Toolbars;
