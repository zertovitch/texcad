
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

--  Warning: This file is automatically generated by AYACC.
--           It is useless to modify it. Change the ".Y" & ".L" files instead.

with YY_Sizes;
-- ^ 14-Jan-2006 (GdM): configurable sizes instead of hard-coded
--   ones in AYACC's output

procedure YYParse is

   -- Rename User Defined Packages to Internal Names.
    package yy_goto_tables         renames
      Gt_Goto;
    package yy_shift_reduce_tables renames
      Gt_Shift_Reduce;
    package yy_tokens              renames
      Gt_Tokens;
    package yy_io                  renames -- (+GdM 2008)
      Gt_IO;

   use yy_tokens, yy_goto_tables, yy_shift_reduce_tables;

   procedure yyerrok;
   procedure yyclearin;


   package yy is

       -- the size of the value and state stacks
       --  Affects error 'Stack size exceeded on state_stack'
       stack_size : constant Natural := yy_sizes.stack_size; -- was 300, then 8192

       -- subtype rule         is natural;
       subtype parse_state  is natural;
       -- subtype nonterminal  is integer;

       -- encryption constants
       default           : constant := -1;
       first_shift_entry : constant :=  0;
       accept_code       : constant := -3001;
       error_code        : constant := -3000;

       -- stack data used by the parser
       tos                : natural := 0;
       value_stack        : array(0..stack_size) of yy_tokens.yystype;
       state_stack        : array(0..stack_size) of parse_state;

       -- current input symbol and action the parser is on
       action             : integer;
       rule_id            : rule;
       input_symbol       : yy_tokens.token:= Error;


       -- error recovery flag
       error_flag : natural := 0;
          -- indicates  3 - (number of valid shifts after an error occurs)

       look_ahead : boolean := true;
       index      : integer;

       -- Is Debugging option on or off
        DEBUG : constant boolean := FALSE;

    end yy;


    function goto_state
      (state : yy.parse_state;
       sym   : nonterminal) return yy.parse_state;

    function parse_action
      (state : yy.parse_state;
       t     : yy_tokens.token) return integer;

    pragma inline(goto_state, parse_action);


    function goto_state(state : yy.parse_state;
                        sym   : nonterminal) return yy.parse_state is
        index : integer;
    begin
        index := goto_offset(state);
        while  integer(goto_matrix(index).nonterm) /= sym loop
            index := index + 1;
        end loop;
        return integer(goto_matrix(index).newstate);
    end goto_state;


    function parse_action(state : yy.parse_state;
                          t     : yy_tokens.token) return integer is
        index      : integer;
        tok_pos    : integer;
        default    : constant integer := -1;
    begin
        tok_pos := yy_tokens.token'pos(t);
        index   := shift_reduce_offset(state);
        while integer(shift_reduce_matrix(index).t) /= tok_pos and then
              integer(shift_reduce_matrix(index).t) /= default
        loop
            index := index + 1;
        end loop;
        return integer(shift_reduce_matrix(index).act);
    end parse_action;

-- error recovery stuff

    procedure handle_error is
      temp_action : integer;
    begin

      if yy.error_flag = 3 then -- no shift yet, clobber input.
      if yy.debug then
          text_io.put_line("  -- Ayacc.YYParse: Error Recovery Clobbers " &
                   yy_tokens.token'image(yy.input_symbol));
      end if;
        if yy.input_symbol = yy_tokens.end_of_input then  -- don't discard,
        if yy.debug then
            text_io.put_line("  -- Ayacc.YYParse: Can't discard END_OF_INPUT, quiting...");
        end if;
        raise yy_tokens.syntax_error;
        end if;

            yy.look_ahead := true;   -- get next token
        return;                  -- and try again...
    end if;

    if yy.error_flag = 0 then -- brand new error
        if yy_io.Input_Line > 1 then
            yyerror("Syntax Error at line" & Text_IO.Count'Image(yy_io.Input_Line));
        else
            yyerror("Syntax Error at line 1 (or possibly later and the AFLex -E option was omitted).");
        end if;
    end if;

    yy.error_flag := 3;

    -- find state on stack where error is a valid shift --

    if yy.debug then
        text_io.put_line("  -- Ayacc.YYParse: Looking for state with error as valid shift");
    end if;

    loop
        if yy.debug then
          text_io.put_line("  -- Ayacc.YYParse: Examining State " &
               yy.parse_state'image(yy.state_stack(yy.tos)));
        end if;
        temp_action := parse_action(yy.state_stack(yy.tos), error);

            if temp_action >= yy.first_shift_entry then
                if yy.tos = yy.stack_size then
                    text_io.put_line("  -- Ayacc.YYParse: Stack size exceeded on state_stack");
                    raise yy_Tokens.syntax_error;
                end if;
                yy.tos := yy.tos + 1;
                yy.state_stack(yy.tos) := temp_action;
                exit;
            end if;

        Decrement_Stack_Pointer :
        begin
          yy.tos := yy.tos - 1;
        exception
          when Constraint_Error =>
            yy.tos := 0;
        end Decrement_Stack_Pointer;

        if yy.tos = 0 then
          if yy.debug then
            text_io.put_line("  -- Ayacc.YYParse: Error recovery popped entire stack, aborting...");
          end if;
          raise yy_tokens.syntax_error;
        end if;
    end loop;

    if yy.debug then
        text_io.put_line("  -- Ayacc.YYParse: Shifted error token in state " &
              yy.parse_state'image(yy.state_stack(yy.tos)));
    end if;

    end handle_error;

   -- print debugging information for a shift operation
   procedure shift_debug(state_id: yy.parse_state; lexeme: yy_tokens.token) is
   begin
       text_io.put_line("  -- Ayacc.YYParse: Shift "& yy.parse_state'image(state_id)&" on input symbol "&
               yy_tokens.token'image(lexeme) );
   end;

   -- print debugging information for a reduce operation
   procedure reduce_debug(rule_id: rule; state_id: yy.parse_state) is
   begin
       text_io.put_line("  -- Ayacc.YYParse: Reduce by rule "&rule'image(rule_id)&" goto state "&
               yy.parse_state'image(state_id));
   end;

   -- make the parser believe that 3 valid shifts have occured.
   -- used for error recovery.
   procedure yyerrok is
   begin
       yy.error_flag := 0;
   end yyerrok;

   -- called to clear input symbol that caused an error.
   procedure yyclearin is
   begin
       -- yy.input_symbol := yylex;
       yy.look_ahead := true;
   end yyclearin;


begin
    -- initialize by pushing state 0 and getting the first input symbol
    yy.state_stack(yy.tos) := 0;


    loop

        yy.index := shift_reduce_offset(yy.state_stack(yy.tos));
        if integer(shift_reduce_matrix(yy.index).t) = yy.default then
            yy.action := integer(shift_reduce_matrix(yy.index).act);
        else
            if yy.look_ahead then
                yy.look_ahead   := false;

                yy.input_symbol := yylex;
            end if;
            yy.action :=
             parse_action(yy.state_stack(yy.tos), yy.input_symbol);
        end if;


        if yy.action >= yy.first_shift_entry then  -- SHIFT

            if yy.debug then
                shift_debug(yy.action, yy.input_symbol);
            end if;

            -- Enter new state
            if yy.tos = yy.stack_size then
                text_io.put_line(" Stack size exceeded on state_stack");
                raise yy_Tokens.syntax_error;
            end if;
            yy.tos := yy.tos + 1;
            yy.state_stack(yy.tos) := yy.action;
              yy.value_stack(yy.tos) := yylval;

        if yy.error_flag > 0 then  -- indicate a valid shift
            yy.error_flag := yy.error_flag - 1;
        end if;

            -- Advance lookahead
            yy.look_ahead := true;

        elsif yy.action = yy.error_code then       -- ERROR

            handle_error;

        elsif yy.action = yy.accept_code then
            if yy.debug then
                text_io.put_line("  -- Ayacc.YYParse: Accepting Grammar...");
            end if;
            exit;

        else -- Reduce Action

            -- Convert action into a rule
            yy.rule_id  := -1 * yy.action;

            -- Execute User Action
            -- user_action(yy.rule_id);


                case yy.rule_id is

when 5 => -- #line 61

		    null;
		

when 9 => -- #line 74

			vcount:= vcount + 1;
			vertex(vcount):= (
yy.value_stack(yy.tos-3).intval, 
yy.value_stack(yy.tos-1).intval, To_Unbounded_String(
yy.value_stack(yy.tos-6).text(1..
yy.value_stack(yy.tos-6).length)));
			max_x:= Integer'Max(max_x, 
yy.value_stack(yy.tos-3).intval);
			max_y:= Integer'Max(max_y, 
yy.value_stack(yy.tos-1).intval);
			begin
              Add(vmap, vcount, 
yy.value_stack(yy.tos-6).text(1..
yy.value_stack(yy.tos-6).length));
			  -- Put_Line("Adding vertex: " & $2.text(1..$2.length) );
			exception
			  when GT_Help.Duplicate_name =>
			    New_Line;
			    Put_Line("Name: [" & 
yy.value_stack(yy.tos-6).text(1..
yy.value_stack(yy.tos-6).length) & ']');
				raise;
		    end;
		

when 10 => -- #line 93
   if first_edge then
		      Start_picture;
			  first_edge:= False;
		    end if;
			current_edge:= (
			  v1 => Index(vmap, 
yy.value_stack(yy.tos-2).text(1..
yy.value_stack(yy.tos-2).length)), 
			  v2 => Index(vmap, 
yy.value_stack(yy.tos).text(1..
yy.value_stack(yy.tos).length)),
			  arrowed => 
yy.value_stack(yy.tos-1).intval /= 1,
			  weight  => 1
			);
			if 
yy.value_stack(yy.tos-1).intval = 2 then -- swap edges
			  current_edge:= (
			    current_edge.v2, current_edge.v1, 
			    current_edge.arrowed, current_edge.weight
		      );
			end if;
		

when 11 => -- #line 111
 
		  Insert_edge(current_edge);
        

when 12 => -- #line 117
 
yyval.intval := 1; 

when 13 => -- #line 118
 
yyval.intval := 2; 

when 14 => -- #line 119
 
yyval.intval := 3; 

when 17 => -- #line 128
 current_edge.weight := 
yy.value_stack(yy.tos).intval; 

                    when others => null;
                end case;


            -- Pop RHS states and goto next state
            yy.tos      := yy.tos - rule_length(yy.rule_id) + 1;
            if yy.tos > yy.stack_size then
                text_io.put_line(" Stack size exceeded on state_stack");
                raise yy_Tokens.syntax_error;
            end if;
            yy.state_stack(yy.tos) := goto_state(yy.state_stack(yy.tos-1) ,
                                 get_lhs_rule(yy.rule_id));

              yy.value_stack(yy.tos) := yyval;

            if yy.debug then
                reduce_debug(yy.rule_id,
                    goto_state(yy.state_stack(yy.tos - 1),
                               get_lhs_rule(yy.rule_id)));
            end if;

        end if;


    end loop;


end yyparse;
