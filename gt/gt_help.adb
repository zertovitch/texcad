with TC.Output;

package body GT_Help is

  procedure Init is
  begin
    vcount:= 0;
    gt_line:= 0;
    max_x:= 0;
    max_y:= 0;
    first_edge:= True;
  end Init;

  procedure Start_picture is
    use TC;
    obj, label: ptr_Obj_type;
  begin
    pic.ul_in_pt:= 1.0;
    pic.lw_in_pt:= 0.4;
    pic.opt.unitlength:= To_Unbounded_String("1pt");
    pic.opt.linewidth:= To_Unbounded_String("0.4pt");
    pic.opt.zoom_fac:= 1.5;
    -- Output all edges
    for i in 1..vcount loop
      -- in GT y=0 is at the top...
      vertex(i).y:= max_y - vertex(i).y;
      obj:= new Obj_type(disc);
      obj.P1:= (Real(vertex(i).x), Real(vertex(i).y));
      obj.rad:= 2.0;
      obj.ls:= normal_line_settings;
      Insert(pic, obj, at_begin);
      label:= new Obj_type(txt);
      label.P1:= obj.P1 + (obj.rad + 2.0, obj.rad + 2.0);
      label.ls:= normal_line_settings;
      label.size:= (Real(Length(vertex(i).name)) * 10.0, 10.0);
      label.inhalt:= vertex(i).name;
      label.adjust:= "ll";
      label.adjust_len:= 2;
      label.solid:= False;
      Insert(pic, label, at_begin);
    end loop;
  end Start_picture;

  procedure Insert_edge(e: Edge) is
    use TC;
    obj: ptr_Obj_type;
  begin
    obj:= new Obj_type(line);
    obj.P1:= (Real(vertex(e.v1).x), Real(vertex(e.v1).y));
    obj.P2:= (Real(vertex(e.v2).x), Real(vertex(e.v2).y));
    obj.ls:= normal_line_settings;
    if e.arrowed then
      obj.ls.arrows:= head;
    end if;
    if e.weight > 1 then
      obj.ls.thickness:= thick;
    end if;
    obj.any_slope:= True;
    Insert(pic, obj, at_begin);
  end Insert_edge;

  procedure Save_picture(name: String) is
  begin
    TC.Output.Save(pic, False, name, "From GraphThing -> " & name);
  end Save_picture;

  procedure Add( to_map: in out Map_of_Vertices; index: Positive; name: String ) is
    pos: Vertex_Mapping.Cursor;
    success: Boolean;
  begin
    Vertex_Mapping.Insert(
                          Vertex_Mapping.Map(to_map),
                          Ada.Strings.Unbounded.To_Unbounded_String(name),
                          index,
                          pos,
                          success
                         );
    if not success then -- A.18.4. 45/2
      raise Duplicate_name with Name;
    end if;
  end Add;

  function Index(from_map: in Map_of_Vertices; name: String ) return Positive is
  begin
    return Vertex_Mapping.Element(
      Vertex_Mapping.Map(from_map),
      Ada.Strings.Unbounded.To_Unbounded_String(name)
    );
  end Index;

end GT_Help;
