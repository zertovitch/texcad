-- Basic tokens
%token NUMBER
%token QSTRING
%token TAG, LBRACE_t, RBRACE_t, EQUAL_t, LPAREN_t, RPAREN_t, COMMA_t

-- Construction non-terminals
-- %type <integer> arrow

-- Keywords
%token kwINFO, kwDESCRIPTION
%token kwVERTEX, kwEDGE, kwAT, kwWITH
%token kwWEIGHT
%token kwDASH, kwLEFT, kwRIGHT
%start input

{

  type const_type is (
    intval,
    floatval,
    doubleval,
    stringval,
    any_type
  );

  type YYSType is record
     text    : String(1..80);
     length  : Natural := 0;
     vartype : const_type;
     intval  : Integer;
     floatval: Long_Float;
  end record;

}

%%	-- Grammar rules and actions follow

-- rc      : RC_items {RC_Help.YY_ACCEPT;}
--         | error    {RC_Help.YY_ABORT;}
--         ;

input:	  info_block objects
	;

info_block:
	  kwINFO 
	  LBRACE_t
		info_tags
	  RBRACE_t
	;

info_tags:
	  -- empty
	| info_tags info_tag
	;

info_tag:
	  TAG 
	  EQUAL_t
	  QSTRING
		{
		    null;
		}
	;

objects:
	  -- empty
	| objects vertex
	| objects edge
	;

vertex:
	  kwVERTEX QSTRING kwAT LPAREN_t NUMBER COMMA_t NUMBER RPAREN_t
		{
			vcount:= vcount + 1;
			vertex(vcount):= ($5.intval, $7.intval, To_Unbounded_String($2.text(1..$2.length)));
			max_x:= Integer'Max(max_x, $5.intval);
			max_y:= Integer'Max(max_y, $7.intval);
			begin
              Add(vmap, vcount, $2.text(1..$2.length));
			  -- Put_Line("Adding vertex: " & $2.text(1..$2.length) );
			exception
			  when GT_Help.Duplicate_name =>
			    New_Line;
			    Put_Line("Name: [" & $2.text(1..$2.length) & ']');
				raise;
		    end;
		}
	;

edge:
	  kwEDGE QSTRING arrow QSTRING
		{   if first_edge then
		      Start_picture;
			  first_edge:= False;
		    end if;
			current_edge:= (
			  v1 => Index(vmap, $2.text(1..$2.length)), 
			  v2 => Index(vmap, $4.text(1..$4.length)),
			  arrowed => $3.intval /= 1,
			  weight  => 1
			);
			if $3.intval = 2 then -- swap edges
			  current_edge:= (
			    current_edge.v2, current_edge.v1, 
			    current_edge.arrowed, current_edge.weight
		      );
			end if;
		}
	  edge_properties
		{ 
		  Insert_edge(current_edge);
        }
	;

arrow:	  
      kwDASH		{ $$.intval := 1; }
	| kwLEFT		{ $$.intval := 2; }
	| kwRIGHT		{ $$.intval := 3; }
	;

edge_properties:
	  -- empty
	| kwWITH edge_property
	;

edge_property:
	  kwWEIGHT NUMBER	{ current_edge.weight := $2.intval; }
	;

%%

-- This header comes from gt.y (bottom)

with GT_Tokens, GT_Shift_Reduce, GT_Goto, GT_Help, GT_IO;
use  GT_Tokens, GT_Shift_Reduce, GT_Goto, GT_Help, GT_IO;

with GT_DFA, YYroutines, YYerror;
use  GT_DFA, YYroutines;

with Ada.Text_IO;                       use Ada.Text_IO;
with Text_IO; -- for compat.

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

with Interfaces;                        use Interfaces;

-- Header end.

##