with Office_Applications;

with Floating_Toolbars;

with GWindows.Base,
     GWindows.Common_Controls,
     GWindows.Image_Lists,
     GWindows.Menus,
     GWindows.Windows;

package TC.GWin.MDI_Main is

  type MDI_Status_bar_part is
    ( modified, command, comment, coords, zoom, stat_objects );

  type MDI_Status_Bar_Type is
    new GWindows.Common_Controls.Status_Bar_Type with null record;

  procedure On_Right_Click (Control : in out MDI_Status_Bar_Type);
  --  Handle right clicks on status bar

  type MDI_Main_Type is
    new Office_Applications.Classic_Main_Window_Type with
      record
        Floating_toolbars      : Floating_toolbar_array;
        Status_Bar             : MDI_Status_Bar_Type;
        File_menu,
        View_Menu              : GWindows.Menus.Menu_Type;
        record_dimensions      : Boolean:= False; -- in On_Move, On_Size
      end record;

  type MDI_Main_Access is access all MDI_Main_Type;

  procedure On_File_New (Window : in out MDI_Main_Type; extra_first: Boolean);
  --  File|New event

  procedure On_File_Open (Window : in out MDI_Main_Type);
  --  File|Open event

  procedure On_About (Window : in out MDI_Main_Type);
  --  Help|About event

  procedure On_Create (Window : in out MDI_Main_Type);
  --  Handles setting up icons, menus, etc.

  procedure On_Size (Window : in out MDI_Main_Type;
                     Width  : in     Integer;
                     Height : in     Integer);

  procedure On_Move (Window : in out MDI_Main_Type;
                     Left   : in     Integer;
                     Top    : in     Integer);

  procedure On_Menu_Hover (Window  : in out MDI_Main_Type;
                           Item    : in     Integer;
                           Kind    : in     GWindows.Windows.Hover_Item_Type);
  --  Handle setting status bar

  procedure On_Menu_Select (Window : in out MDI_Main_Type;
                            Item   : in     Integer);
  --  Handle standard menu selections

  procedure On_File_Drop (Window     : in out MDI_Main_Type;
                          File_Names : in     GWindows.Windows.Array_Of_File_Names);

  overriding procedure On_Close (Window    : in out MDI_Main_Type;
                                 Can_Close :    out Boolean);

  procedure Update_Common_Menus (Window        : in out MDI_Main_Type;
                                 top_mru_entry :        GString := "" );

  procedure Update_Status_Bar(
    Window    : in out MDI_Main_Type;
    Part      :        MDI_Status_bar_part;
    Content   :        GString:= "" );

  -- 13-Aug-2004
  procedure Toolbar_enabling(
    window    : in out MDI_Main_Type;
    switch    :        Boolean
  );

  procedure Redraw_all (Window: in out MDI_Main_Type);

  -- 13-Aug-2004
  procedure Show_Dialog_with_Toolbars_off
    (Window : in     GWindows.Base.Base_Window_Type'Class;
     Parent : in     GWindows.Base.Base_Window_Type'Class;
     Main   : in out MDI_Main_Type;
     Result :    out Integer
  );

end TC.GWin.MDI_Main;
