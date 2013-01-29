-- %option noyywrap case-sensitive

LWS		[ \t]+
CRRET		\r
NEWLINE		\n
DIGIT		[0-9]
LBRACE [{]
RBRACE [}]
LPAREN [(]
RPAREN [)]
EQUAL [=]

TAG		[A-Za-z0-9_]+

DEC_NUMBER	{DIGIT}+

QSTRING		\"(([^"])|(\\\"))*\"

--************** Start of lexical rules **************
%%

-- Kill all LWS
--  {LWS}
--  {CRRET}

{LBRACE}         { return LBRACE_t; }
{RBRACE}         { return RBRACE_t; }
{EQUAL}          { return EQUAL_t; }
{LPAREN} {return LPAREN_t;}
{RPAREN} {return RPAREN_t;}
, { return COMMA_t; }

-- Treat newlines (almost) the same as LWS
{NEWLINE}	{ gt_line:= gt_line + 1; }

-- Integer constants
{DEC_NUMBER}	{
			yylval.intval := Integer'Value(yytext);
			return NUMBER;
		}

-- Quoted Strings
{QSTRING}	{
            -- We unquote here.
			yylval.text(1..yylength-2):= yytext(2..yylength-1);
			yylval.length:= yylength-2;
			return QSTRING;
		}

-- Reserved keywords
"info"		{ return kwINFO; }
"vertex"	{ return kwVERTEX; }
"edge"		{ return kwEDGE; }
"at"		{ return kwAT; }
"with"		{ return kwWITH; }
"weight"	{ return kwWEIGHT; }

"--"		{ return kwDASH; }
"<-"		{ return kwLEFT; }
"->"		{ return kwRIGHT; }

{TAG}		{
              null;
			-- int len = strlen (yytext);

			-- yylval.str = new char [len+1];
			-- memcpy (yylval.str, yytext, len);
			-- yylval.str[len] = 0;
			return TAG;
		}

-- Any other character
-- .		{ return NUMBER; -- !! Bogus }

-- "("                { return '('; }
-- ")"                { return ')'; }
-- "}"                { return '}'; }
-- "{"                { return '{'; }

%%
--************** End of lexical rules **************

with GT_Tokens; use GT_Tokens;
with GT_Help;   use GT_Help;

##
