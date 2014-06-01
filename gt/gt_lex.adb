--  Warning: This lexical scanner is automatically generated by AFLEX.
--           It is useless to modify it. Change the ".Y" & ".L" files instead.
with Text_IO; use Text_IO;
with gt_dfa; use gt_dfa; 
with gt_io; use gt_io; 
--# line 1 "gt.l"
-- %option noyywrap case-sensitive
--************** Start of lexical rules **************
--# line 20 "gt.l"

--************** End of lexical rules **************

with GT_Tokens; use GT_Tokens;
with GT_Help;   use GT_Help;

function gt_Lex return Token is
subtype Short is Integer range -32768..32767;
    yy_act : Integer;
    yy_c   : Short;

-- returned upon end-of-file
YY_END_TOK : constant Integer := 0;
YY_END_OF_BUFFER : constant := 21;
subtype yy_state_type is Integer;
yy_current_state : yy_state_type;
INITIAL : constant := 0;
yy_accept : constant array(0..51) of Short :=
    (   0,
        0,    0,   21,   20,    7,   20,    4,    5,    6,   20,
        8,   20,    3,   19,   19,   19,   19,   19,   19,    1,
        2,    0,    9,    0,   16,   18,    8,   19,   17,   13,
       19,   19,   19,   19,   19,    9,   19,   19,   19,   19,
       19,   12,   10,   19,   19,   14,   19,   19,   11,   15,
        0
    ) ;

yy_ec : constant array(ASCII.NUL..Character'Last) of short :=
    (   0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    2,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    3,    1,    1,    1,    1,    1,    4,
        5,    1,    1,    6,    7,    1,    1,    8,    8,    8,
        8,    8,    8,    8,    8,    8,    8,    1,    1,    9,
       10,   11,    1,    1,   14,   13,   13,   15,   16,   17,
       18,   19,   20,   13,   13,   13,   13,   21,   22,   13,
       13,   23,   13,   24,   13,   25,   26,   27,   13,   13,
        1,   12,    1,    1,   13,    1,   14,   13,   13,   15,

       16,   17,   18,   19,   20,   13,   13,   13,   13,   21,
       22,   13,   13,   23,   13,   24,   13,   25,   26,   27,
       13,   13,   28,    1,   29,    1,    1, others=> 1

    ) ;

yy_meta : constant array(0..29) of short :=
    (   0,
        1,    1,    1,    1,    1,    1,    1,    2,    1,    1,
        1,    1,    2,    2,    2,    2,    2,    2,    2,    2,
        2,    2,    2,    2,    2,    2,    2,    1,    1
    ) ;

yy_base : constant array(0..53) of Short :=
    (   0,
        0,    0,   66,   67,   67,   27,   67,   67,   67,   24,
       57,   57,   67,    0,   39,   47,   40,   44,   16,   67,
       67,   30,   67,   31,   67,   67,   51,    0,   67,    0,
       40,   40,   33,   35,   30,   34,   37,   30,   27,   32,
       30,    0,    0,   29,   25,    0,   14,   16,    0,    0,
       67,   46,   36
    ) ;

yy_def : constant array(0..53) of Short :=
    (   0,
       51,    1,   51,   51,   51,   52,   51,   51,   51,   51,
       53,   51,   51,   53,   53,   53,   53,   53,   53,   51,
       51,   52,   51,   52,   51,   51,   53,   53,   51,   53,
       53,   53,   53,   53,   53,   52,   53,   53,   53,   53,
       53,   53,   53,   53,   53,   53,   53,   53,   53,   53,
        0,   51,   51
    ) ;

yy_nxt : constant array(0..96) of Short :=
    (   0,
        4,    5,    6,    7,    8,    9,   10,   11,   12,   13,
        4,    4,   14,   15,   14,   16,   14,   14,   14,   17,
       14,   14,   14,   14,   18,   19,   14,   20,   21,   23,
       25,   34,   23,   36,   26,   35,   23,   28,   24,   50,
       49,   24,   24,   48,   47,   24,   22,   22,   46,   45,
       44,   43,   42,   41,   40,   39,   38,   37,   27,   33,
       32,   31,   30,   29,   27,   51,    3,   51,   51,   51,
       51,   51,   51,   51,   51,   51,   51,   51,   51,   51,
       51,   51,   51,   51,   51,   51,   51,   51,   51,   51,
       51,   51,   51,   51,   51,   51

    ) ;

yy_chk : constant array(0..96) of Short :=
    (   0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    6,
       10,   19,   22,   24,   10,   19,   36,   53,    6,   48,
       47,   22,   24,   45,   44,   36,   52,   52,   41,   40,
       39,   38,   37,   35,   34,   33,   32,   31,   27,   18,
       17,   16,   15,   12,   11,    3,   51,   51,   51,   51,
       51,   51,   51,   51,   51,   51,   51,   51,   51,   51,
       51,   51,   51,   51,   51,   51,   51,   51,   51,   51,
       51,   51,   51,   51,   51,   51

    ) ;


-- copy whatever the last rule matched to the standard output

procedure ECHO is
begin
   if Text_IO.is_open(user_output_file) then
     Text_IO.put( user_output_file, yytext );
   else
     Text_IO.put( yytext );
   end if;
end ECHO;

-- enter a start condition.
-- Using procedure requires a () after the ENTER, but makes everything
-- much neater.

procedure ENTER( state : integer ) is
begin
     yy_start := 1 + 2 * state;
end ENTER;

-- action number for EOF rule of a given start state
function YY_STATE_EOF(state : integer) return integer is
begin
     return YY_END_OF_BUFFER + state + 1;
end YY_STATE_EOF;

-- return all but the first 'n' matched characters back to the input stream
procedure yyless(n : integer) is
begin
        yy_ch_buf(yy_cp) := yy_hold_char; -- undo effects of setting up yytext
        yy_cp := yy_bp + n;
        yy_c_buf_p := yy_cp;
        YY_DO_BEFORE_ACTION; -- set up yytext again
end yyless;

-- redefine this if you have something you want each time.
procedure YY_USER_ACTION is
begin
        null;
end;

-- yy_get_previous_state - get the state just before the EOB char was reached

function yy_get_previous_state return yy_state_type is
    yy_current_state : yy_state_type;
    yy_c : short;
begin
    yy_current_state := yy_start;

    for yy_cp in yytext_ptr..yy_c_buf_p - 1 loop
	yy_c := yy_ec(yy_ch_buf(yy_cp));
	if yy_accept(yy_current_state) /= 0 then
	    yy_last_accepting_state := yy_current_state;
	    yy_last_accepting_cpos := yy_cp;
	end if;
	while yy_chk(yy_base(yy_current_state) + yy_c) /= yy_current_state loop
	    yy_current_state := yy_def(yy_current_state);
	    if ( yy_current_state >= 52 ) then
		yy_c := yy_meta(yy_c);
	    end if;
	end loop;
	yy_current_state := yy_nxt(yy_base(yy_current_state) + yy_c);
    end loop;

    return yy_current_state;
end yy_get_previous_state;

procedure yyrestart( input_file : file_type ) is
begin
   open_input(Text_IO.name(input_file));
end yyrestart;

begin -- of YYLex
<<new_file>>
        -- this is where we enter upon encountering an end-of-file and
        -- yywrap() indicating that we should continue processing

    if yy_init then
        if yy_start = 0 then
            yy_start := 1;      -- first start state
        end if;

        -- we put in the '\n' and start reading from [1] so that an
        -- initial match-at-newline will be true.

        yy_ch_buf(0) := ASCII.LF;
        yy_n_chars := 1;

        -- we always need two end-of-buffer characters. The first causes
        -- a transition to the end-of-buffer state. The second causes
        -- a jam in that state.

        yy_ch_buf(yy_n_chars) := YY_END_OF_BUFFER_CHAR;
        yy_ch_buf(yy_n_chars + 1) := YY_END_OF_BUFFER_CHAR;

        yy_eof_has_been_seen := false;

        yytext_ptr := 1;
        yy_c_buf_p := yytext_ptr;
        yy_hold_char := yy_ch_buf(yy_c_buf_p);
        yy_init := false;
-- UMASS CODES :
--   Initialization
        tok_begin_line := 1;
        tok_end_line := 1;
        tok_begin_col := 0;
        tok_end_col := 0;
        token_at_end_of_line := false;
        line_number_of_saved_tok_line1 := 0;
        line_number_of_saved_tok_line2 := 0;
-- END OF UMASS CODES.
    end if; -- yy_init

    loop                -- loops until end-of-file is reached

-- UMASS CODES :
--    if last matched token is end_of_line, we must
--    update the token_end_line and reset tok_end_col.
    if Token_At_End_Of_Line then
      Tok_End_Line := Tok_End_Line + 1;
      Tok_End_Col := 0;
      Token_At_End_Of_Line := False;
    end if;
-- END OF UMASS CODES.

        yy_cp := yy_c_buf_p;

        -- support of yytext
        yy_ch_buf(yy_cp) := yy_hold_char;

        -- yy_bp points to the position in yy_ch_buf of the start of the
        -- current run.
	yy_bp := yy_cp;
	yy_current_state := yy_start;
	loop
		yy_c := yy_ec(yy_ch_buf(yy_cp));
		if yy_accept(yy_current_state) /= 0 then
		    yy_last_accepting_state := yy_current_state;
		    yy_last_accepting_cpos := yy_cp;
		end if;
		while yy_chk(yy_base(yy_current_state) + yy_c) /= yy_current_state loop
		    yy_current_state := yy_def(yy_current_state);
		    if ( yy_current_state >= 52 ) then
			yy_c := yy_meta(yy_c);
		    end if;
		end loop;
		yy_current_state := yy_nxt(yy_base(yy_current_state) + yy_c);
	    yy_cp := yy_cp + 1;
if ( yy_current_state = 51 ) then
    exit;
end if;
	end loop;
	yy_cp := yy_last_accepting_cpos;
	yy_current_state := yy_last_accepting_state;

<<next_action>>
	    yy_act := yy_accept(yy_current_state);
            YY_DO_BEFORE_ACTION;
            YY_USER_ACTION;

        if aflex_debug then  -- output acceptance info. for (-d) debug mode
            Text_IO.put( Standard_Error, "--accepting rule #" );
            Text_IO.put( Standard_Error, INTEGER'IMAGE(yy_act) );
            Text_IO.put_line( Standard_Error, "(""" & yytext & """)");
        end if;

-- UMASS CODES :
--   Update tok_begin_line, tok_end_line, tok_begin_col and tok_end_col
--   after matching the token.
        if yy_act /= YY_END_OF_BUFFER and then yy_act /= 0 then
-- Token are matched only when yy_act is not yy_end_of_buffer or 0.
          Tok_Begin_Line := Tok_End_Line;
          Tok_Begin_Col := Tok_End_Col + 1;
          Tok_End_Col := Tok_Begin_Col + yy_cp - yy_bp - 1;
          if yy_ch_buf ( yy_bp ) = ASCII.LF then
            Token_At_End_Of_Line := True;
          end if;
        end if;
-- END OF UMASS CODES.

<<do_action>>   -- this label is used only to access EOF actions
            case yy_act is
		when 0 => -- must backtrack
		-- undo the effects of YY_DO_BEFORE_ACTION
		yy_ch_buf(yy_cp) := yy_hold_char;
		yy_cp := yy_last_accepting_cpos;
		yy_current_state := yy_last_accepting_state;
		goto next_action;



-- Kill all LWS
--  {LWS}
--  {CRRET}
when 1 => 
--# line 26 "gt.l"
 return LBRACE_t; 

when 2 => 
--# line 27 "gt.l"
 return RBRACE_t; 

when 3 => 
--# line 28 "gt.l"
 return EQUAL_t; 

when 4 => 
--# line 29 "gt.l"
return LPAREN_t;

when 5 => 
--# line 30 "gt.l"
return RPAREN_t;

when 6 => 
--# line 31 "gt.l"
 return COMMA_t; 

-- Treat newlines (almost) the same as LWS
when 7 => 
--# line 34 "gt.l"
 gt_line:= gt_line + 1; 

-- Integer constants
when 8 => 
--# line 37 "gt.l"

			yylval.intval := Integer'Value(yytext);
			return NUMBER;
		

-- Quoted Strings
when 9 => 
--# line 43 "gt.l"

            -- We unquote here.
			yylval.text(1..yylength-2):= yytext(2..yylength-1);
			yylval.length:= yylength-2;
			return QSTRING;
		

-- Reserved keywords
when 10 => 
--# line 51 "gt.l"
 return kwINFO; 

when 11 => 
--# line 52 "gt.l"
 return kwVERTEX; 

when 12 => 
--# line 53 "gt.l"
 return kwEDGE; 

when 13 => 
--# line 54 "gt.l"
 return kwAT; 

when 14 => 
--# line 55 "gt.l"
 return kwWITH; 

when 15 => 
--# line 56 "gt.l"
 return kwWEIGHT; 

when 16 => 
--# line 58 "gt.l"
 return kwDASH; 

when 17 => 
--# line 59 "gt.l"
 return kwLEFT; 

when 18 => 
--# line 60 "gt.l"
 return kwRIGHT; 

when 19 => 
--# line 62 "gt.l"

              null;
			-- int len = strlen (yytext);

			-- yylval.str = new char [len+1];
			-- memcpy (yylval.str, yytext, len);
			-- yylval.str[len] = 0;
			return TAG;
		

-- Any other character
-- .		{ return NUMBER; -- !! Bogus }
-- "("                { return '('; }
-- ")"                { return ')'; }
-- "}"                { return '}'; }
-- "{"                { return '{'; }
when 20 => 
--# line 80 "gt.l"
ECHO;
when YY_END_OF_BUFFER + INITIAL + 1 => 
    return End_Of_Input;
                when YY_END_OF_BUFFER =>
                    -- undo the effects of YY_DO_BEFORE_ACTION
                    yy_ch_buf(yy_cp) := yy_hold_char;

                    yytext_ptr := yy_bp;

                    case yy_get_next_buffer is
                        when EOB_ACT_END_OF_FILE =>
                            begin
                            if yywrap then
                                -- note: because we've taken care in
                                -- yy_get_next_buffer() to have set up yytext,
                                -- we can now set up yy_c_buf_p so that if some
                                -- total hoser (like aflex itself) wants
                                -- to call the scanner after we return the
                                -- End_Of_Input, it'll still work - another
                                -- End_Of_Input will get returned.

                                yy_c_buf_p := yytext_ptr;

                                yy_act := YY_STATE_EOF((yy_start - 1) / 2);

                                goto do_action;
                            else
                                --  start processing a new file
                                yy_init := true;
                                goto new_file;
                            end if;
                            end;
                        when EOB_ACT_RESTART_SCAN =>
                            yy_c_buf_p := yytext_ptr;
                            yy_hold_char := yy_ch_buf(yy_c_buf_p);
                        when EOB_ACT_LAST_MATCH =>
                            yy_c_buf_p := yy_n_chars;
                            yy_current_state := yy_get_previous_state;

                            yy_cp := yy_c_buf_p;
                            yy_bp := yytext_ptr;
                            goto next_action;
                        when others => null;
                        end case; -- case yy_get_next_buffer()
                when others =>
                    Text_IO.put( "action # " );
                    Text_IO.put( INTEGER'IMAGE(yy_act) );
                    Text_IO.new_line;
                    raise AFLEX_INTERNAL_ERROR;
            end case; -- case (yy_act)
        end loop; -- end of loop waiting for end of file
end gt_Lex;
--# line 80 "gt.l"