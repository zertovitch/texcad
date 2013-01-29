with TC;

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

package GT_Help is

  type Vertex_type is record
    x, y: Integer;
    name: Unbounded_String;
  end record;

  vertex: array(1..1_000_000) of Vertex_type;

  vcount: Natural;
  gt_line: Natural;
  max_x, max_y: Natural;
  first_edge: Boolean;
  -- GraphThing saves all vertices, then edges.
  -- We can memorize only vertices, and then spit edges directly.
  -- Actually the grammar allows mixing vertices and edges, but pshhht ;-).

  package Vertex_Mapping is new Ada.Containers.Hashed_Maps
     (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type    => Positive,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => Ada.Strings.Unbounded."=");

  type Map_of_Vertices is new Vertex_Mapping.Map with null record;

  procedure Add( to_map: in out Map_of_Vertices; index: Positive; name: String );
  Duplicate_name: exception;

  function Index( from_map: in Map_of_Vertices; name: String ) return Positive;

  vmap: Map_of_Vertices;

  procedure Init;

  pic: TC.Picture;

  procedure Start_picture;
  procedure Save_picture(name: String);

  type Edge is record
    v1, v2 : Positive;
    arrowed: Boolean;  -- arrow from v1 to v2
    weight : Positive;
  end record;

  procedure Insert_edge(e: Edge);

  current_edge: Edge;

  syntax_error: exception;

end GT_Help;
