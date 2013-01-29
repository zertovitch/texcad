package body GT_Help is

  procedure Init is
  begin
    vcount:= 0;
    gt_line:= 0;
	max_x:= 0;
	max_y:= 0;
  end Init;

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
