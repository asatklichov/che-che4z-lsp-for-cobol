/*
 * Copyright (c) 2020 Broadcom.
 * The term "Broadcom" refers to Broadcom Inc. and/or its subsidiaries.
 *
 * This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License 2.0
 * which is available at https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *   Broadcom, Inc. - initial API and implementation
 */
    
lexer grammar CobolLexer;
import CICSLexer;
channels{TECHNICAL}

TITLESTATEMENT : (T I T L E ' '+ .*? NEWLINE) -> skip;

LEVEL_NUMBER: ([1-9])|([0][1-9])|([1234][0-9]);
LEVEL_NUMBER_66 : '66';
LEVEL_NUMBER_77 : '77';
LEVEL_NUMBER_88 : '88';

// keywords
ACCEPT : A C C E P T;
ADVANCING : A D V A N C I N G;
ALIGNED : A L I G N E D;
ALPHABET : A L P H A B E T;
ALPHABETIC : A L P H A B E T I C;
ALPHABETIC_LOWER : A L P H A B E T I C MINUSCHAR L O W E R;
ALPHABETIC_UPPER : A L P H A B E T I C MINUSCHAR U P P E R;
ALPHANUMERIC : A L P H A N U M E R I C;
ALPHANUMERIC_EDITED : A L P H A N U M E R I C MINUSCHAR E D I T E D;
ALSO : A L S O;
ALTERNATE : A L T E R N A T E;
APPLY : A P P L Y;
AREA : A R E A;
AREAS : A R E A S;
ASCENDING : A S C E N D I N G;
ASSOCIATED_DATA : A S S O C I A T E D MINUSCHAR D A T A;
ASSOCIATED_DATA_LENGTH : A S S O C I A T E D MINUSCHAR D A T A MINUSCHAR L E N G T H;
AUTHOR : A U T H O R;
AUTO_SKIP : A U T O MINUSCHAR S K I P;
BACKGROUND_COLOR : B A C K G R O U N D MINUSCHAR C O L O R;
BACKGROUND_COLOUR : B A C K G R O U N D MINUSCHAR C O L O U R;
BASIS : B A S I S;
BEGINNING : B E G I N N I N G;
BLANK : B L A N K;
BLOCK : B L O C K;
BOTTOM : B O T T O M;
BOUNDS : B O U N D S;
BYFUNCTION : B Y F U N C T I O N;
BYTITLE : B Y T I T L E;
CANCEL : C A N C E L;
CAPABLE : C A P A B L E;
CCSVERSION : C C S V E R S I O N;
CHAINING : C H A I N I N G;
CHARACTERS : C H A R A C T E R S;
CICS : C I C S;
CLASS_ID : C L A S S MINUSCHAR I D;
CLOCK_UNITS : C L O C K MINUSCHAR U N I T S;
CLOSE : C L O S E;
CLOSE_DISPOSITION : C L O S E MINUSCHAR D I S P O S I T I O N;
CODE : C O D E;
CODE_SET : C O D E MINUSCHAR S E T;
COLLATING : C O L L A T I N G;
COMMA : C O M M A;
COMMITMENT : C O M M I T M E N T;
COMMON : C O M M O N;
COMP : C O M P;
COMPUTATIONAL : C O M P U T A T I O N A L;
COMPUTATIONAL_1 : C O M P U T A T I O N A L MINUSCHAR '1';
COMPUTATIONAL_2 : C O M P U T A T I O N A L MINUSCHAR '2';
COMPUTATIONAL_3 : C O M P U T A T I O N A L MINUSCHAR '3';
COMPUTATIONAL_4 : C O M P U T A T I O N A L MINUSCHAR '4';
COMPUTATIONAL_5 : C O M P U T A T I O N A L MINUSCHAR '5';
COMPUTE : C O M P U T E;
COMP_1 : C O M P MINUSCHAR '1';
COMP_2 : C O M P MINUSCHAR '2';
COMP_3 : C O M P MINUSCHAR '3';
COMP_4 : C O M P MINUSCHAR '4';
COMP_5 : C O M P MINUSCHAR '5';
COM_REG : C O M MINUSCHAR R E G;
CONFIGURATION : C O N F I G U R A T I O N;
CONTENT : C O N T E N T;
CONTROLS : C O N T R O L S;
CONTROL_POINT : C O N T R O L MINUSCHAR P O I N T;
CONVENTION : C O N V E N T I O N;
CONVERTING : C O N V E R T I N G;
COPYENTRY : ('*>CPYENTER<URI>' .*? '</URI>') -> channel(TECHNICAL);
COPYEXIT : '*>CPYEXIT' + NEWLINE -> channel(TECHNICAL);
CR : C R;
CRUNCH : C R U N C H;
CURRENCY : C U R R E N C Y;
DATE_COMPILED : D A T E MINUSCHAR C O M P I L E D;
DATE_WRITTEN : D A T E MINUSCHAR W R I T T E N;
DAY_OF_WEEK : D A Y MINUSCHAR O F MINUSCHAR W E E K;
DBCS : D B C S;
DEBUGGING : D E B U G G I N G;
DEBUG_CONTENTS : D E B U G MINUSCHAR C O N T E N T S;
DEBUG_ITEM : D E B U G MINUSCHAR I T E M;
DEBUG_LINE : D E B U G MINUSCHAR L I N E;
DEBUG_NAME : D E B U G MINUSCHAR N A M E;
DEBUG_SUB_1 : D E B U G MINUSCHAR S U B MINUSCHAR '1';
DEBUG_SUB_2 : D E B U G MINUSCHAR S U B MINUSCHAR '2';
DEBUG_SUB_3 : D E B U G MINUSCHAR S U B MINUSCHAR '3';
DECIMAL_POINT : D E C I M A L MINUSCHAR P O I N T;
DECLARATIVES : D E C L A R A T I V E S;
DEFAULT_DISPLAY : D E F A U L T MINUSCHAR D I S P L A Y;
DELIMITED : D E L I M I T E D;
DEPENDING : D E P E N D I N G;
DESCENDING : D E S C E N D I N G;
DFHRESP : D F H R E S P;
DFHVALUE : D F H V A L U E;
DISK : D I S K;
DISPLAY : D I S P L A Y;
DISPLAY_1 : D I S P L A Y MINUSCHAR '1';
DIVIDE : D I V I D E;
DIVISION : D I V I S I O N;
DOWN : D O W N;
DUPLICATES : D U P L I C A T E S;
EGCS : E G C S;
EGI : E G I;
EJECT: E J E C T -> channel(HIDDEN);
EJECT_DOT: E J E C T DOT_FS-> channel(HIDDEN);
EMI : E M I;
ENCODING: E N C O D I N G;
ENDING : E N D I N F;
END_ACCEPT : E N D MINUSCHAR A C C E P T;
END_ADD : E N D MINUSCHAR A D D;
END_CALL : E N D MINUSCHAR C A L L;
END_COMPUTE : E N D MINUSCHAR C O M P U T E;
END_DELETE : E N D MINUSCHAR D E L E T E;
END_DIVIDE : E N D MINUSCHAR D I V I D E;
END_EVALUATE : E N D MINUSCHAR E V A L U A T E;
END_IF : E N D MINUSCHAR I F;
END_MULTIPLY : E N D MINUSCHAR M U L T I P L Y;
END_OF_PAGE : E N D MINUSCHAR O F MINUSCHAR P A G E;
END_PERFORM : E N D MINUSCHAR P E R F O R M;
END_READ : E N D MINUSCHAR R E A D;
END_RECEIVE : E N D MINUSCHAR R E C E I V E;
END_RETURN : E N D MINUSCHAR R E T U R N;
END_REWRITE : E N D MINUSCHAR R E W R I T E;
END_SEARCH : E N D MINUSCHAR S E A R C H;
END_START : E N D MINUSCHAR S T A R T;
END_STRING : E N D MINUSCHAR S T R I N G;
END_SUBTRACT : E N D MINUSCHAR S U B T R A C T;
END_UNSTRING : E N D MINUSCHAR U N S T R I N G;
END_WRITE : E N D MINUSCHAR W R I T E;
END_XML : E N D MINUSCHAR X M L;
EOP : E O P;
ESI : E S I;
EVALUATE : E V A L U A T E;
EVERY : E V E R Y;
EXHIBIT : E X H I B I T;
EXTEND : E X T E N D;
FD : F D;
FILE_CONTROL : F I L E MINUSCHAR C O N T R O L;
FILLER : F I L L E R;
FOOTING : F O O T I N G;
FUNCTION_POINTER : F U N C T I O N MINUSCHAR P O I N T E R;
GIVING : G I V I N G;
GOBACK : G O B A C K;
GREATER : G R E A T E R;
GROUP_USAGE: G R O U P MINUSCHAR U S A G E;
HIGHLIGHT : H I G H L I G H T;
HIGH_VALUE : H I G H MINUSCHAR V A L U E;
HIGH_VALUES : H I G H MINUSCHAR V A L U E S;
ID : I D;
IDENTIFICATION : I D E N T I F I C A T I O N;
IMPLICIT : I M P L I C I T;
INDEXED : I N D E X E D;
INDICATE : I N D I C A T E;
INITIAL : I N I T I A L;
INITIALIZE : I N I T I A L I Z E;
INITIATE : I N I T I A T E;
INPUT_OUTPUT : I N P U T MINUSCHAR O U T P U T;
INSPECT : I N S P E C T;
INSTALLATION : I N S T A L L A T I O N;
I_O : I MINUSCHAR O;
I_O_CONTROL : I MINUSCHAR O MINUSCHAR C O N T R O L;
JUST : J U S T;
JUSTIFIED : J U S T I F I E D;
KANJI : K A N J I;
KEPT : K E P T;
KEYBOARD : K E Y B O A R D;
LESS : L E S S;
LINAGE : L I N A G E;
LINAGE_COUNTER : L I N A G E MINUSCHAR C O U N T E R;
LINES : L I N E S;
LINE_COUNTER : L I N E MINUSCHAR C O U N T E R;
LINKAGE : L I N K A G E;
LOCAL_STORAGE : L O C A L MINUSCHAR S T O R A G E;
LONG_DATE : L O N G MINUSCHAR D A T E;
LONG_TIME : L O N G MINUSCHAR T I M E;
LOWER : L O W E R;
LOW_VALUE : L O W MINUSCHAR V A L U E;
LOW_VALUES : L O W MINUSCHAR V A L U E S;
MEMORY : M E M O R Y;
MODULES : M O D U L E S;
MORE_LABELS : M O R E MINUSCHAR L A B E L S;
MULTIPLE : M U L T I P L E;
MULTIPLY : M U L T I P L Y;
NATIONAL_EDITED : N A T I O N A L MINUSCHAR E D I T E D;
NATIVE : N A T I V E;
NEGATIVE : N E G A T I V E;
NETWORK : N E T W O R K;
NUMERIC_DATE : N U M E R I C MINUSCHAR D A T E;
NUMERIC_EDITED : N U M E R I C MINUSCHAR E D I T E D;
NUMERIC_TIME : N U M E R I C MINUSCHAR T I M E;
OBJECT_COMPUTER : O B J E C T MINUSCHAR C O M P U T E R;
OCCURS : O C C U R S;
ODT : O D T;
OMITTED : O M I T T E D;
ORDERLY : O R D E R L Y;
OTHER : O T H E R;
OVERFLOW : O V E R F L O W;
OWN : O W N;
PACKED_DECIMAL : P A C K E D MINUSCHAR D E C I M A L;
PADDING : P A D D I N G;
PAGE_COUNTER : P A G E MINUSCHAR C O U N T E R;
PARSE: P A R S E;
PERFORM : P E R F O R M;
PIC : P I C  -> pushMode(PICTURECLAUSE);
PICTURE : P I C T U R E -> pushMode(PICTURECLAUSE);
POINTER : P O I N T E R;
PORT : P O R T;
POSITIVE : P O S I T I V E;
PRINTER : P R I N T E R;
PRINTING : P R I N T I N G;
PROCEDURES : P R O C E D U R E S;
PROCEDURE_POINTER : P R O C E D U R E MINUSCHAR P O I N T E R;
PROCEED : P R O C E E D;
PROCESSING: P R O C E S S I N G;
PROGRAM_ID : P R O G R A M MINUSCHAR I D;
QUOTE : Q U O T E;
QUOTES : Q U O T E S;
RANDOM : R A N D O M;
READER : R E A D E R;
RECEIVED : R E C E I V E D;
RECORDING : R E C O R D I N G;
RECORDS : R E C O R D S;
RECURSIVE : R E C U R S I V E;
REDEFINES : R E D E F I N E S;
REEL : R E E L;
RELOAD: R E L O A D;
REMAINDER : R E M A I N D E R;
REMOTE : R E M O T E;
REMOVAL : R E M O V A L;
RENAMES : R E N A M E S;
REPLACING : R E P L A C I N G;
REPORT : R E P O R T;
REPORTING : R E P O R T I N G;
REPORTS : R E P O R T S;
RERUN : R E R U N;
RESERVE : R E S E R V E;
RETURNING: R E T U R N I N G;
RETURN_CODE : R E T U R N MINUSCHAR C O D E;
REVERSED : R E V E R S E D;
ROUNDED : R O U N D E D;
SAME : S A M E;
SAVE : S A V E;
SD : S D;
SEGMENT : S E G M E N T;
SEGMENT_LIMIT : S E G M E N T MINUSCHAR L I M I T;
SENTENCE : S E N T E N C E;
SEPARATE : S E P A R A T E;
SEQUENTIAL : S E Q U E N T I A L;
SERVICE: S E R V I C E;
SHIFT_IN : S H I F T MINUSCHAR I N;
SHIFT_OUT : S H I F T MINUSCHAR O U T;
SHORT_DATE : S H O R T MINUSCHAR D A T E;
SIGN : S I G N;
SKIP1 : S K I P '1' -> skip;
SKIP2 : S K I P '2' -> skip;
SKIP3 : S K I P '3' -> skip;
SORT : S O R T;
SORT_CONTROL : S O R T MINUSCHAR C O N T R O L;
SORT_CORE_SIZE : S O R T MINUSCHAR C O R E MINUSCHAR S I Z E;
SORT_FILE_SIZE : S O R T MINUSCHAR F I L E MINUSCHAR S I Z E;
SORT_MERGE : S O R T MINUSCHAR M E R G E;
SORT_MESSAGE : S O R T MINUSCHAR M E S S A G E;
SORT_MODE_SIZE : S O R T MINUSCHAR M O D E MINUSCHAR S I Z E;
SORT_RETURN : S O R T MINUSCHAR R E T U R N;
SOURCE_COMPUTER : S O U R C E MINUSCHAR C O M P U T E R;
SPACES : S P A C E S;
SPECIAL_NAMES : S P E C I A L MINUSCHAR N A M E S;
SQLIMS : S Q L I M S;
STANDARD : S T A N D A R D;
STANDARD_1 : S T A N D A R D MINUSCHAR '1';
STANDARD_2 : S T A N D A R D MINUSCHAR '2';
STOP : S T O P;
STRING : S T R I N G;
SUBTRACT : S U B T R A C T;
SUM : S U M;
SUPPRESS : S U P P R E S S;
SYMBOLIC : S Y M B O L I C;
SYNC : S Y N C;
SYNCHRONIZED : S Y N C H R O N I Z E D;
TALLY : T A L L Y;
TALLYING : T A L L Y I N G;
TAPE : T A P E;
TERMINATE : T E R M I N A T E;
THAN : T H A N;
THREAD : T H R E A D;
THREAD_LOCAL : T H R E A D MINUSCHAR L O C A L;
THROUGH : T H R O U G H;
THRU : T H R U;
TIMES : T I M E S;
TODAYS_DATE : T O D A Y S MINUSCHAR D A T E;
TODAYS_NAME : T O D A Y S MINUSCHAR N A M E;
TOP : T O P;
TRUNCATED : T R U N C A T E D;
TYPEDEF : T Y P E D E F;
UNIT : U N I T;
UNSTRING : U N S T R I N G;
UP : U P;
UPON : U P O N;
VALIDATING: V A L I D A T I N G;
VIRTUAL : V I R T U A L;
WHEN_COMPILED : W H E N MINUSCHAR C O M P I L E D;
WORDS : W O R D S;
WORKING_STORAGE : W O R K I N G MINUSCHAR S T O R A G E;
WRITE_ONLY : W R I T E MINUSCHAR O N L Y;
ZERO : Z E R O;
ZEROES : Z E R O E S;
ZEROS : Z E R O S;

mode PICTURECLAUSE;
FINALCHARSTRING: CHARSTRING+ ->popMode;
CHARSTRING: PICTURECHARSGROUP1+ PICTURECHARSGROUP2? LParIntegralRPar? '.'? (PICTURECHARSGROUP1|PICTURECHARSGROUP2)
			PICTURECHARSGROUP1+ PICTURECHARSGROUP2? LParIntegralRPar?|
			PICTURECHARSGROUP1* '.' PICTUREPeriodAcceptables+ LParIntegralRPar?|
			PICTURECHARSGROUP1* PICTURECHARSGROUP2? PICTURECHARSGROUP1+ LParIntegralRPar? '.'? (PICTURECHARSGROUP1|PICTURECHARSGROUP2)|
			PICTURECHARSGROUP1* PICTURECHARSGROUP2? PICTURECHARSGROUP1+ LParIntegralRPar?|
			PICTURECHARSGROUP2 PICTURECHARSGROUP1* LParIntegralRPar? '.'? (PICTURECHARSGROUP1|PICTURECHARSGROUP2)|
			PICTURECHARSGROUP2 PICTURECHARSGROUP1* LParIntegralRPar?
;

DOT_FS2 : '.' ('\r' | '\n' | '\f' | '\t' | ' ')+ -> popMode;
PICTURECHARSGROUP1: PICTURECharAcceptedMultipleTime+;
PICTURECHARSGROUP2: PICTURECharAcceptedOneTime+;
WS2 : [ \t\f;]+ -> channel(HIDDEN);
IS2: I S;
LParIntegralRPar: LPARENCHAR INTEGERLITERAL RPARENCHAR;
fragment PICTUREPeriodAcceptables: ('0'|'9'|B|Z|CR|DB|ASTERISKCHAR|COMMACHAR|MINUSCHAR|PLUSCHAR|SLASHCHAR);
fragment PICTURECharAcceptedMultipleTime: (A|G|N|P|X|DOLLARCHAR|PICTUREPeriodAcceptables);
fragment PICTURECharAcceptedOneTime: (V|E|S|CR|DB);
