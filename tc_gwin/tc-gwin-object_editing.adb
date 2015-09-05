with TC.GWin.Lang;                      use TC.GWin.Lang;
with TeXCAD_Resource_GUI;               use TeXCAD_Resource_GUI;

with GWindows.Buttons;                  use GWindows.Buttons;
with GWindows.Constants;                use GWindows.Constants;
with GWindows.Edit_Boxes;               use GWindows.Edit_Boxes;
with GWindows.Static_Controls;          use GWindows.Static_Controls;
with GWindows.Windows;                  use GWindows.Windows;

with GWin_Util;                         use GWin_Util;

with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;

package body TC.GWin.Object_editing is

  use TC.RIO;

  procedure Change_Text(
    parent  : in out Base_Window_Type'Class;
    main    : in out MDI_Main_Type;
    t       : in out obj_type;
    modified:    out Boolean ) is

    pan                : Window_Type;
    text_eb, dash_eb   : Edit_Box_Type;
    h_group            : Group_Box_Type;
    h_radio            : array(H_Justify) of Radio_Button_Type;
    v_group            : Group_Box_Type;
    v_radio            : array(V_Justify) of Radio_Button_Type;
    oki                : Default_Button_Type;
    cancel             : Button_Type;
    Result,w,x,y,wprop : Integer;

    h_just: H_Justify;
    v_just: V_Justify;
    candidate: obj_type(t.art);
    dum_str: String(1..20);

    h_adj_groups: constant:= 95;

    function Panel_title return String is
    begin
      case t.art is
        when putaux => return "\put(x,y){...}";
        when txt    => return "\makebox(0,0){...}";
        when box    =>
          case t.ls.pattern is
            when plain => return "\framebox(){...}";
            when dot   => return "%\dottedbox(){...}";
            when dash  => return "\dashbox(){...}";
          end case;
        when others => return obj_art_type'Image(t.art); -- should not happen!
      end case;
    end Panel_title;

    procedure Get_Box_Data
      (Window : in out GWindows.Base.Base_Window_Type'Class)
    is
      pragma Warnings(off,window);
    begin
      candidate.inhalt:= To_Unbounded_String(GWindows.Edit_Boxes.Text(text_eb));
      if t.art /= putaux then
        for h in H_Justify loop
          if State(h_radio(h))=checked then
            h_just:= h;
          end if;
        end loop;
        for v in V_Justify loop
          if State(v_radio(v))=checked then
            v_just:= v;
          end if;
        end loop;
        Images( h_just, v_just, candidate.adjust, candidate.adjust_len );
      end if;
      if t.ls.pattern = dash then
        candidate.ls.dash_length:= TC.Real'Value(Text(dash_eb));
      end if;
    end Get_Box_Data;

  begin
    Values( t.adjust(1..t.adjust_len), h_just, v_just );

    y:= 0;
    if t.art /= putaux then
      y:= h_adj_groups;
    end if;

    if t.ls.pattern /= plain then
      y:= y + 30;
    end if;

    Create_As_Dialog(pan, parent, Panel_title, Width => 630, Height => 110+y);

    Center(pan);
    Small_Icon (Pan, "Options_Icon");
    On_Destroy_Handler (pan, Get_Box_Data'Unrestricted_Access);

    GWin_Util.Use_GUI_Font(pan);

    w:= Client_Area_Width (pan) - 20;

    Create(text_eb, pan, To_String(t.inhalt), 10, 8, w, 24);

    if t.art /= putaux then

      wprop:= (w*2)/3;

      Create(h_group, pan, "", 10, 30, wprop - 5, h_adj_groups);

      for h in H_Justify loop
        x:= 20 + 120* H_justify'Pos(h);
        Create_Label (pan,
          Msg(message'Val(message'Pos(left)+H_justify'Pos(h))),
          x,  70, 120, 40);
        Create(h_radio(h),pan, "",  x,  45,  80, 15);
        State(h_radio(h),boolean_to_state(h=h_just));
      end loop;

      x:= 10 + 5 + wprop + 5;

      Create(v_group, pan, "", x, 30, (w-wprop) - 10, h_adj_groups);

      for v in V_Justify loop
        y:= 45 + 25* V_justify'Pos(v);
        Create_Label (pan,
           Msg(message'Val(message'Pos(top)+V_justify'Pos(v))),
           x + 50,  y, 130, 20);
        Create(v_radio(v),pan, "",  x+10,  y,  30, 15);
        State(v_radio(v),boolean_to_state(v=v_just));
      end loop;

    end if;
    y:= y + 40;
    case t.ls.pattern is
      when plain => null;
      when dash =>
        Create_Label(pan,Msg(dash_size), 10,  y, 130, 20);
        Put(dum_str,t.ls.dash_length,2,0);
        Create (dash_eb, pan, Trim(dum_str,left), 180, y,  40, 20);
      when dot => null;
    end case;

    Create (oki, pan, "O&K", 20,
            Client_Area_Height (pan) - 40, 60, 25, ID => IDOK);
    Create (cancel, pan, Msg(mcancel), 100,
            Client_Area_Height (pan) - 40, 60, 25, ID => IDCANCEL);

    candidate:= t;

    Focus(text_eb);

    Show_Dialog_with_Toolbars_off(pan, parent, main, result);

    case Result is
      when IDOK     =>
        modified:=
          t.adjust(1..t.adjust_len) /=
            candidate.adjust(1..candidate.adjust_len) or else
          t.inhalt /= candidate.inhalt or else
          (t.ls.pattern = dash and then
           candidate.ls.dash_length /= t.ls.dash_length);
        t:= candidate;
      when others   =>
        modified:= False; -- Contains IDCANCEL
    end case;

  end Change_Text;

  procedure Change_Oval(
    parent  : in out Base_Window_Type'Class;
    main    : in out MDI_Main_Type;
    t       : in out obj_type;
    modified:    out Boolean ) is

    pan      : Window_Type;
    bordcoin : array( Ovop ) of Dialog_Button_Type;
    result,i : Integer;
    nov      : Ovop;
    ID_offset: constant:= 10000;

  begin
    Create_As_Dialog(pan, parent, "\oval", Width => 460, Height => 225);
    Center(pan);
    Small_Icon (Pan, "Options_Icon");
    GWin_Util.Use_GUI_Font(pan);

    for o in Ovop loop
      i:= Ovop'Pos(o);
      Create(
        bordcoin(o), pan,
        To_Lower( S2G(Ovop'Image(o)) ),
        100 + (i mod 3) * 100,
        30  + (i / 3) * 50,
        60, 25, ID => i+ID_offset );
    end loop;
    Focus(bordcoin(t.part));

    Show_Dialog_with_Toolbars_off(pan, parent, main, result);

    if result = IDCANCEL then
      modified:= False;
    else
      i:= result-ID_offset;
      -- 13-Jan-2004: sometimes command outside of range.
      if i in Ovop'Pos(Ovop'First) .. Ovop'Pos(Ovop'Last) then
        nov:= Ovop'Val(i);
      else
        nov:= Entire;
      end if;
      modified:= t.part /= nov;
      t.part:= nov;
    end if;

  end Change_Oval;

  procedure Change_Bezier(
    parent  : in out Base_Window_Type'Class;
    main    : in out MDI_Main_Type;
    ul_in_pt:        Real;
    t       : in out Obj_type;
    modified:    out Boolean ) is

    pan                : Window_Type;
    pts_box            : Edit_Box_Type;
    qbezier_radio      : array( Boolean ) of Radio_Button_Type;
    oki                : Default_Button_Type;
    cancel             : Button_Type;
    Result             : Integer;
    candidate, i0      : Natural;

    procedure Get_Data
      (Window : in out GWindows.Base.Base_Window_Type'Class)
    is
      pragma Warnings(off,window);
      i   : Integer;
    begin
      if State(qbezier_radio(True))=checked then
        candidate:= 0;
      else
        Focus(pts_box);
        i:= Integer'Value(Text(pts_box));
        if i >= 1 then
          candidate:= i;
        end if;
      end if;
    exception
      when others => null; -- Wrong data
    end Get_Data;

  begin
    Create_As_Dialog(pan, parent, "\qbezier, \bezier", Width => 320, Height => 140);
    Center(pan);
    Small_Icon (Pan, "Options_Icon");
    On_Destroy_Handler (pan, Get_Data'Unrestricted_Access);
    GWin_Util.Use_GUI_Font(pan);

    candidate:= t.num;
    if candidate = 0 then
      i0:= Good_num_of_bezier_points(t,ul_in_pt);
    else
      i0:= candidate;
    end if;

    Create(qbezier_radio(True),pan, "",       10,  20,  20, 20);
    Create_Label( pan, "Auto  (\qbezier)",    40,  20, 140, 20);
    Create(qbezier_radio(False),pan, "",     190,  20,  20, 20);
    Create(pts_box, pan, Integer'Image(i0),  220,  20,  60, 20);

    State(qbezier_radio(True),boolean_to_state(candidate=0));
    State(qbezier_radio(False),boolean_to_state(candidate/=0));

    Create (oki, pan, "O&K", 20,
            Client_Area_Height (pan) - 40, 60, 25, ID => IDOK);
    Create (cancel, pan, Msg(mcancel), 100,
            Client_Area_Height (pan) - 40, 60, 25, ID => IDCANCEL);

    Focus(pts_box);

    Show_Dialog_with_Toolbars_off(pan, parent, main, result);

    case Result is
      when IDOK     => modified:= candidate /= t.num;
                       t.num:= candidate;
      when others   => modified:= False; -- Contains IDCANCEL
    end case;

  end Change_Bezier;

  ---------------------------------------------
  --  2D parametric curve properties dialog  --
  ---------------------------------------------

  procedure Change_Param_2D(
    parent  : in out Base_Window_Type'Class;
    main    : in out MDI_Main_Type;
    t       : in out Obj_type;
    modified:    out Boolean )
  is
    pan       : TeXCAD_Resource_GUI.Param_Curve_2D_Dialog_Type;
    Result    : Integer;
    candidate : Param_curve_2D_data:= t.data_2d;
    valid     : Boolean:= False;

    procedure Set_Data is
    begin
      pan.Segments_Box.Text(Trim(Integer'Image(candidate.segments), Left));
      pan.Scale_Box.Text(TeX_Number(candidate.scale));
      pan.X_Form_Box.Text(To_String(candidate.form_x));
      pan.Y_Form_Box.Text(To_String(candidate.form_y));
      pan.T_Min_Box.Text(TeX_Number(candidate.min_t));
      pan.T_Max_Box.Text(TeX_Number(candidate.max_t));
    end Set_Data;

    procedure Get_Data
      (Window : in out GWindows.Base.Base_Window_Type'Class)
    is
      pragma Warnings(off,window);
    begin
      candidate:=
        ( segments => Natural'Value(pan.Segments_Box.Text),
          scale    => TeX_Number(pan.Scale_Box.Text),
          form_x   => U(pan.X_Form_Box.Text),
          form_y   => U(pan.Y_Form_Box.Text),
          min_t    => TeX_Number(pan.T_Min_Box.Text),
          max_t    => TeX_Number(pan.T_Max_Box.Text)
        );
      valid:= True;
    exception
      when others => null; -- Wrong data
    end Get_Data;

  begin
    Create_Full_Dialog(pan, Parent, Msg(param2d_title));
    Center(pan);
    Small_Icon (Pan, "Options_Icon");
    On_Destroy_Handler (pan, Get_Data'Unrestricted_Access);
    Set_Data;
    pan.Segments_Label.Text(Msg(param2d_segments));
    pan.Scale_Label.Text(Msg(param2d_scale));
    pan.IDCANCEL.Text(Msg(mcancel));
    Show_Dialog_with_Toolbars_off(pan, parent, main, result);

    case Result is
      when IDOK     =>
        if valid then
          modified:= candidate /= t.data_2d;
          if modified then
            t.data_2d:= candidate;
            t.parsed_2d_x.Parse(t.data_2d.form_x);
            t.parsed_2d_y.Parse(t.data_2d.form_y);
          end if;
        end if;
      when others   => modified:= False; -- Contains IDCANCEL
    end case;
  end Change_Param_2D;

end TC.GWin.Object_editing;
