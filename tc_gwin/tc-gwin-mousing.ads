with TC.GWin.MDI_Picture_Child;         use TC.GWin.MDI_Picture_Child;

with GWindows.Cursors;                  use GWindows.Cursors;
with GWindows.Windows;                  use GWindows.Windows;

pragma Elaborate_All(GWindows.Cursors); -- Load_Cursor

package TC.GWin.Mousing is

  cur_select    : Cursor_Type:= Load_Cursor("Select_cursor");
  cur_unselect  : Cursor_Type:= Load_Cursor("Unselect_cursor");
  cur_pick      : Cursor_Type:= Load_Cursor("Pick_cursor");
  cur_unpick    : Cursor_Type:= Load_Cursor("Unpick_cursor");
  cur_picking   : Cursor_Type:= Load_Cursor("Picking_cursor");
  cur_chg_text  : Cursor_Type:= Load_Cursor("Text_Change_cursor");
  cur_arrow     : Cursor_Type:= Load_System_Cursor (IDC_ARROW);
  cur_set_origin: Cursor_Type:= Load_Cursor("Set_origin_cursor");

  procedure Show_coordinates(w: TC_Picture_Panel);
  procedure Show_mouse_mode(w: TC_Picture_Panel);

  procedure Tranform_coordinates(w: in out TC_Picture_Panel);

  procedure Change_Cursor(w: in out TC_Picture_Panel; cur: Cursor_Type);

  procedure Mouse_Down (w    : in out TC_Picture_Panel;
                        X, Y : in     Integer;
                        Btn  : in     Mouse_Keys );

  procedure Mouse_Move (w    : in out TC_Picture_Panel;
                        X, Y : in     Integer);

  procedure Mouse_Up (w    : in out TC_Picture_Panel;
                      X, Y : in     Integer);

end TC.GWin.Mousing;
