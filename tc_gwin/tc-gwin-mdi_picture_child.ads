with TC.GWin.MDI_Main;

with Office_Applications;

with GWindows.Base,
     GWindows.Cursors,
     GWindows.Drawing,
     GWindows.Drawing_Objects,
     GWindows.Drawing_Panels,
     GWindows.Menus,
     GWindows.Types;

package TC.GWin.MDI_Picture_Child is

  type MDI_Picture_Child_Type;
  type MDI_Picture_Child_Access is access all MDI_Picture_Child_Type;

  type Capture_mode is
    (none, pick, unpick, select_area, unselect_area,
     click_1,  -- Figure on one point (text, putaux)
     figure_2, -- Figure with two ends, in one, long, click
     click_2,  -- Supplemental line, click on the end
     bez_click0, bez_click1, bez_click2,
     paste0, paste1);

  type TC_Picture_Panel is
    new GWindows.Drawing_Panels.Drawing_Panel_Type with
      record
        Picture        : TC.Picture;
        Drawing_Area   : GWindows.Drawing.Canvas_Type;
        Saved_Area     : GWindows.Drawing.Memory_Canvas_Type;
        Saved_Bitmap   : GWindows.Drawing_Objects.Bitmap_Type;
        Cursor         : GWindows.Cursors.Cursor_Type;
        X0, Y0         : Integer := 0;  --  Origin of displayed part in Canvas pixels
        Xs, Ys         : Integer := 0;  --  Start in Canvas pixels (1st end)
        PS             : Point;         --  Start in picture units
        X, Y           : Integer;       --  Cursor in Canvas pixels
        PU             : Point;         --  Cursor in picture units
        Xb, Yb         : Integer := 0;  --  Point 2 for Bezier
        PE             : Point;         --  Point 2 for Bezier in picture units
        Disp_W,
        Disp_H         : Integer;       --  Size of displayed area (=client area of parent)
        pic_parent     : MDI_Picture_Child_Access;  --  -> containing window
        main           : MDI_Main.MDI_Main_Access;  --  -> main window
        hor_splitt,
        ver_splitt,                --  Aufteilung des Bildschirms
        curs_x, curs_y : Integer;  --  Cursorposition in Bildschirm-Koord.
        wx, wy,                    --  Cursorposition in Weltkoordinaten
        m_wx, m_wy     : TC.Real;  --  maximales x und y in Weltkoordinaten
        current_cmd    : Permanent_cmd;
        current_ls     : Line_Settings;
        capture        : Capture_mode;
        phantomart     : Obj_art_type;
        phantom_ls     : Line_Settings;  --  sometimes /= current_ls (e.g. translation vector)
      end record;

  procedure Subtle_Redraw (Window : in out TC_Picture_Panel);

  overriding procedure On_Paint (Window : in out TC_Picture_Panel;
                                 Canvas : in out GWindows.Drawing.Canvas_Type;
                                 Area   : in     GWindows.Types.Rectangle_Type);

  type MDI_Picture_Child_Type is
    new Office_Applications.Classic_Document_Window_Type with
      record
        --  Access to the main, containing window:
        mdi_root     : MDI_Main.MDI_Main_Access;
        ID            : ID_Type;
        --  ^ Window title = ID.short_name & {""|" *"}
        Macro_Name   : GWindows.GString_Unbounded;  --  a macro to be loaded
        --  Scroll_Panel : GWindows.Scroll_Panels.Scroll_Panel_Type;
        Draw_Control : TC_Picture_Panel;
        File_Menu,
        Draw_Menu,
        Line_Menu,
        Edit_Menu,
        View_Menu    : GWindows.Menus.Menu_Type;
      end record;

  overriding procedure On_Horizontal_Scroll
    (Window  : in out MDI_Picture_Child_Type;
     Request : in     GWindows.Base.Scroll_Request_Type;
     Control : in     GWindows.Base.Pointer_To_Base_Window_Class);

  overriding procedure On_Vertical_Scroll
    (Window  : in out MDI_Picture_Child_Type;
     Request : in     GWindows.Base.Scroll_Request_Type;
     Control : in     GWindows.Base.Pointer_To_Base_Window_Class);

  overriding procedure On_Size (Window : in out MDI_Picture_Child_Type;
                                Width  : in     Integer;
                                Height : in     Integer);

  --  Added 17-May-2004: avoid painting the background colour since the
  --  area is completely covered by the canvas; practically, it avoids
  --  the flickering when resizing the window with full redraw option

  overriding procedure On_Erase_Background
    (Window : in out MDI_Picture_Child_Type;
     Canvas : in out GWindows.Drawing.Canvas_Type;
     Area   : in     GWindows.Types.Rectangle_Type);

  procedure Save (Window    : in out MDI_Picture_Child_Type;
                  File_Name : in     GWindows.GString;
                  macro     : in     Boolean);

  procedure On_Save (Window : in out MDI_Picture_Child_Type);

  procedure On_Save_As (Window : in out MDI_Picture_Child_Type; macro : Boolean);
  --  Handles file saves

  --  procedure On_Pre_Create (Window    : in out MDI_Picture_Child_Type;
  --                           dwStyle   : in out Interfaces.C.unsigned;
  --                           dwExStyle : in out Interfaces.C.unsigned);

  overriding procedure On_Create (Window : in out MDI_Picture_Child_Type);
  --  Handles creating window

  procedure Create_TeXCAD_MDI_Child
    (Window : in out MDI_Picture_Child_Type;
     Parent : in out MDI_Main.MDI_Main_Type;
     ID     : in     ID_Type);

  overriding procedure On_Menu_Select
    (Window : in out MDI_Picture_Child_Type;
     Item   : in     Integer);
  --  Handles menu selections

  overriding procedure On_Close (Window    : in out MDI_Picture_Child_Type;
                                 Can_Close :    out Boolean);

  overriding function Is_Document_Modified (Window : MDI_Picture_Child_Type) return Boolean;

  overriding procedure On_Focus (Window : in out MDI_Picture_Child_Type);

  --  For when the MDI parent tries to close its children.
  success_in_enumerated_close : Boolean;

  procedure Zoom_Picture
    (Window                : in out MDI_Picture_Child_Type;
     exponential_direction : in     Integer);

  procedure Update_Information (Window : in out MDI_Picture_Child_Type);

  procedure Update_Permanent_Command (Window : in out MDI_Picture_Child_Type);

  procedure Load_Macro (Window : in out MDI_Picture_Child_Type);

end TC.GWin.MDI_Picture_Child;
