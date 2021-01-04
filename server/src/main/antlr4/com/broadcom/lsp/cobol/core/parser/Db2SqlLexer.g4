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
lexer grammar Db2SqlLexer;
import CommonCobolLexer;


// DB2 SQL Reserved Keywords
ABSOLUTE : A B S O L U T E;
ACCELERATION : A C C E L E R A T I O N;
ACCELERATOR : A C C E L E R A T O R;
ACCESS : A C C E S S;
ACCESSCTRL : A C C E S S C T R L;
ACCTNG: A C C T N G;
ACTIVATE : A C T I V A T E;
ACTIVE : A C T I V E;
ADA : A D A;
AGE: A G E;
ALGORITHM : A L G O R I T H M;
ALIAS : A L I A S;
ALL : A L L;
ALLOW : A L L O W;
ALTERIN : A L T E R I N;
ALWAYS : A L W A Y S;
ANY : A N Y;
APPEND : A P P E N D;
APPLCOMPAT : A P P L C O M P A T;
APPLICATION : A P P L I C A T I O N;
APPLNAME : A P P L N A M E;
APPLY : A P P L Y;
ARCHIVE : A R C H I V E;
ARE : A R E;
ARRAY: A R R A Y;
ARRAY_EXISTS : A R R A Y UNDERSCORECHAR E X I S T S;
ASC : A S C;
ASCII : A S C I I;
ASENSITIVE : A S E N S I T I V E;
ASSEMBLE : A S S E M B L E;
ASSERTION : A S S E R T I O N;
ASSOCIATE : A S S O C I A T E;
ASUTIME : A S U T I M E;
ATOMIC : A T O M I C;
AUDIT : A U D I T;
AUTHENTICATION : A U T H E N T I C A T I O N;
AUTHID : A U T H I D;
AUTHORIZATION : A U T H O R I Z A T I O N;
AUTOMATIC : A U T O M A T I C;
AUTONOMOUS : A U T O N O M O U S;
AUX : A U X;
AVG : A V G;
BASED : B A S E D;
BEFORE : B E F O R E;
BEGIN : B E G I N;
BETWEEN : B E T W E E N;
BIGINT : B I G I N T;
BINARY_STRING_CONSTANT : BXNUMBER;
BIND : B I N D;
BINDADD : B I N D A D D;
BINDAGENT : B I N D A G E N T;
BIT_LENGTH: B I T UNDERSCORECHAR L E N G T H;
BLOB : B L O B;
BLOCKED : B L O C K E D;
BOTH : B O T H;
BSDS : B S D S;
BUFFERPOOL : B U F F E R P O O L;
BUFFERPOOLS : B U F F E R P O O L S;
BUSINESS_TIME : B U S I N E S S UNDERSCORECHAR T I M E;
BY : B Y;
//C_LANG : C;
CACHE: C A C H E;
CALL : C A L L;
CALLED : C A L L E D;
CALLER : C A L L E R;
CAPTURE : C A P T U R E;
CARDINALITY : C A R D I N A L I T Y;
CASCADE : C A S C A D E;
CASCADED : C A S C A D E D;
CASE : C A S E;
CAST : C A S T;
CATALOG : C A T A L O G;
CATALOG_NAME: C A T A L O G UNDERSCORECHAR N A M E;
CHANGED : C H A N G E D;
CHANGES : C H A N G E S;
CHAR_LENGTH : C H A R UNDERSCORECHAR L E N G T H;
CHARACTER : C H A R A C T E R;
CHARACTER_LENGTH : C H A R A C T E R UNDERSCORECHAR L E N G T H;
CHECKED : C H E C K E D;
CLAUSE : C L A U S E;
CLIENT_ACCTNG : CLIENT UNDERSCORECHAR ACCTNG; //SPECIAL REGITERS ?
CLIENT_APPLNAME: CLIENT UNDERSCORECHAR APPLNAME;
CLIENT_CORR_TOKEN: CLIENT UNDERSCORECHAR CORR UNDERSCORECHAR TOKEN;
CLIENT_USERID: CLIENT UNDERSCORECHAR USERID;
CLIENT_WRKSTNNAME: CLIENT UNDERSCORECHAR WRKSTNNAME;
CLOB : C L O B;
CLONE : C L O N E;
CLUSTER : C L U S T E R;
COALESCE : C O A L E S C E;
COBOL : C O B O L;
CODEUNITS32 : C O D E U N I T S '3' '2';
COLLATE : C O L L A T E;
COLLATION : C O L L A T I O N;
COLLECT : C O L L E C T;
COLLECTION : C O L L E C T I O N;
COLLID : C O L L I D;
COLUMN : C O L U M N;
COLUMNS : C O L U M N S;
COMMENT : C O M M E N T;
COMMIT : C O M M I T;
COMMITTED: C O M M I T T E D;
COMPARISONS : C O M P A R I S O N S;
COMPATIBILITY : C O M P A T I B I L I T Y;
COMPRESS : C O M P R E S S;
CONCAT : C O N C A T;
CONCENTRATE : C O N C E N T R A T E;
CONCURRENT : C O N C U R R E N T;
CONDITION_NUMBER : C O N D I T I O N UNDERSCORECHAR N U M B E R;
CONNECTION : C O N N E C T I O N;
CONNECTION_NAME : C O N N E C T I O N UNDERSCORECHAR N A M E;
CONSERVATIVE : C O N S E R V A T I V E;
CONSTANT : C O N S T A N T;
CONSTRAINT : C O N S T R A I N T;
CONSTRAINTS : C O N S T R A I N T S;
CONTAINS : C O N T A I N S;
CONTEXT : C O N T E X T;
CONTINUE : C O N T I N U E;
CONVERT : C O N V E R T;
CORR : C O R R;
CORRELATION : C O R R E L A T I O N;
CORRESPONDING : C O R R E S P O N D I N G;
COUNT : C O U N T;
COUNT_BIG : C O U N T UNDERSCORECHAR B I G;
COVARIANCE: C O V A R I A N C E;
CPU :  C P U;
CREATE_SECURE_OBJECT : C R E A T E UNDERSCORECHAR S E C U R E UNDERSCORECHAR O B J E C T;
CREATEALIAS : C R E A T E A L I A S;
CREATEDBA : C R E A T E D B A;
CREATEDBC : C R E A T E S G;
CREATEIN : C R E A T E I N;
CREATESG : C R E A T E S G;
CREATETAB : C R E A T E T A B;
CREATETMTAB : C R E A T E T M T A B;
CREATETS : C R E A T E T S;
CROSS : C R O S S;
CS : C S;
CUBE : C U B E;
CUME_DIST: C U M E UNDERSCORECHAR D I S T;
CURRENT_DATE : C U R R E N T UNDERSCORECHAR D A T E;
CURRENT_LC_CTYPE : CURRENT UNDERSCORECHAR LC_CTYPE;
CURRENT_PATH : C U R R E N T UNDERSCORECHAR P A T H;
CURRENT_SCHEMA : C U R R E N T UNDERSCORECHAR S C H E M A;
CURRENT_SERVER : C U R R E N T UNDERSCORECHAR SERVER;
CURRENT_SQLID : C U R R E N T UNDERSCORECHAR S Q L I D;
CURRENT_TIME : C U R R E N T UNDERSCORECHAR TIME;
CURRENT_TIMESTAMP : C U R R E N T UNDERSCORECHAR TIMESTAMP;
CURRENT_TIMEZONE : C U R R E N T UNDERSCORECHAR TIMEZONE;
CURRENT_USER : C U R R E N T UNDERSCORECHAR U S E R;
CURRENTLY : C U R R E N T L Y;
CURSOR : C U R S O R;
CURSOR_NAME : C U R S O R UNDERSCORECHAR N A M E;
CURSORS : C U R S O R S;
CYCLE : C Y C L E;
DATAACCESS : D A T A A C C E S S;
DATABASE : D A T A B A S E;
DATACLAS: D A T A C L A S;
DATALINK : D A T A L I N K;
DAY : D A Y;
DB : D B;
DB2 : D B '2;';
DB2_AUTHENTICATION_TYPE : D B '2' UNDERSCORECHAR A U T H E N T I C A T I O N UNDERSCORECHAR T Y P E;
DB2_AUTHORIZATION_ID : D B '2' UNDERSCORECHAR A U T H O R  I Z A T I O N UNDERSCORECHAR I D;
DB2_CONNECTION_STATE : D B '2' UNDERSCORECHAR C O N N E C T I O N UNDERSCORECHAR S T A T E;
DB2_CONNECTION_STATUS : D B '2' UNDERSCORECHAR C O N N E C T I O N UNDERSCORECHAR S T A T U S;
DB2_ENCRYPTION_TYPE : D B '2' UNDERSCORECHAR E N C R Y P T I O N UNDERSCORECHAR T Y P E;
DB2_ERROR_CODE1 : D B '2' UNDERSCORECHAR E R R O R UNDERSCORECHAR C O D E '1';
DB2_ERROR_CODE2 : D B '2' UNDERSCORECHAR E R R O R UNDERSCORECHAR C O D E '2';
DB2_ERROR_CODE3 : D B '2' UNDERSCORECHAR E R R O R UNDERSCORECHAR C O D E '3';
DB2_ERROR_CODE4 : D B '2' UNDERSCORECHAR E R R O R UNDERSCORECHAR C O D E '4';
DB2_GET_DIAGNOSTICS_DIAGNOSTICS : D B '2' UNDERSCORECHAR G E T UNDERSCORECHAR D I A G N O S T I C S UNDERSCORECHAR D I A G N O S T I C S;
DB2_INTERNAL_ERROR_POINTER : D B '2' UNDERSCORECHAR I N T E R N A L UNDERSCORECHAR E R R O R UNDERSCORECHAR P O I N T E R;
DB2_LAST_ROW : D B '2' UNDERSCORECHAR L A S T UNDERSCORECHAR R O W;
DB2_LINE_NUMBER : D B '2' UNDERSCORECHAR L I N E UNDERSCORECHAR N U M B E R;
DB2_MESSAGE_ID : D B '2' UNDERSCORECHAR M E S S A G E UNDERSCORECHAR I D;
DB2_MODULE_DETECTING_ERROR : D B '2' UNDERSCORECHAR M O D U L E UNDERSCORECHAR D E T E C T I N G UNDERSCORECHAR E R R O R;
DB2_NUMBER_PARAMETER_MARKERS : D B '2' UNDERSCORECHAR N U M B E R UNDERSCORECHAR P A R A M E T E R UNDERSCORECHAR M A R K E R S;
DB2_NUMBER_RESULT_SETS : D B '2' UNDERSCORECHAR N U M B E R UNDERSCORECHAR R E S U L T UNDERSCORECHAR S E T S;
DB2_NUMBER_ROWS : D B '2' UNDERSCORECHAR N U M B E R UNDERSCORECHAR ROWS;
DB2_ORDINAL_TOKEN1 : D B '2' UNDERSCORECHAR O R D I N A L UNDERSCORECHAR T O K E N '1';
DB2_ORDINAL_TOKEN2 : D B '2' UNDERSCORECHAR O R D I N A L UNDERSCORECHAR T O K E N '2';
DB2_ORDINAL_TOKEN3 : D B '2' UNDERSCORECHAR O R D I N A L UNDERSCORECHAR T O K E N '3';
DB2_ORDINAL_TOKEN4: D B '2' UNDERSCORECHAR O R D I N A L UNDERSCORECHAR T O K E N '4';
DB2_PRODUCT_ID : D B '2' UNDERSCORECHAR P R O D U C T UNDERSCORECHAR I D;
DB2_REASON_CODE : D B '2' UNDERSCORECHAR R E A S O N UNDERSCORECHAR C O D E;
DB2_RETURN_STATUS : D B '2' UNDERSCORECHAR R E T U R N UNDERSCORECHAR S T A T U S;
DB2_RETURNED_SQLCODE : D B '2' UNDERSCORECHAR R E T U R N E D UNDERSCORECHAR S Q L C O D E;
DB2_ROW_NUMBER : D B '2' UNDERSCORECHAR R O W UNDERSCORECHAR N U M B E R;
DB2_SERVER_CLASS_NAME : D B '2' UNDERSCORECHAR S E R V E R UNDERSCORECHAR C L A S S UNDERSCORECHAR N A M E;
DB2_SQL_ATTR_CURSOR_HOLD : D B '2' UNDERSCORECHAR S Q L UNDERSCORECHAR A T T R UNDERSCORECHAR C U R S O R UNDERSCORECHAR H O L D;
DB2_SQL_ATTR_CURSOR_ROWSET : D B '2' UNDERSCORECHAR S Q L UNDERSCORECHAR A T T R  C U R S O R UNDERSCORECHAR R O W S E T;
DB2_SQL_ATTR_CURSOR_SCROLLABLE : D B '2' UNDERSCORECHAR S Q L UNDERSCORECHAR A T T R UNDERSCORECHAR C U R S OR UNDERSCORECHAR S C R O L L A B L E;
DB2_SQL_ATTR_CURSOR_SENSITIVITY : D B '2' UNDERSCORECHAR S Q L UNDERSCORECHAR A T T R UNDERSCORECHAR C U R S O R UNDERSCORECHAR S E N S I T I V I T Y;
DB2_SQL_ATTR_CURSOR_TYPE : D B '2' UNDERSCORECHAR S Q L UNDERSCORECHAR A T T R UNDERSCORECHAR C U R S O R UNDERSCORECHAR T Y P E;
DB2_SQL_NESTING_LEVEL : D B '2' UNDERSCORECHAR S Q L UNDERSCORECHAR N E S T I N G UNDERSCORECHAR L E V E L;
DB2_SQLERRD1 : D B '2' UNDERSCORECHAR S Q L E R R D '1';
DB2_SQLERRD2 : D B '2' UNDERSCORECHAR S Q L E R R D '2';
DB2_SQLERRD3 : D B '2' UNDERSCORECHAR S Q L E R R D '3';
DB2_SQLERRD4 : D B '2' UNDERSCORECHAR S Q L E R R D '4';
DB2_SQLERRD5 : D B '2' UNDERSCORECHAR S Q L E R R D '5';
DB2_SQLERRD6 : D B '2' UNDERSCORECHAR S Q L E R R D  '6';
DB2_SQLERRD_SET : D B '2' UNDERSCORECHAR S Q L E R R D UNDERSCORECHAR S E T;
DB2_TOKEN_COUNT : D B '2' UNDERSCORECHAR T O K E N UNDERSCORECHAR C O U N T;
DB2DARI : D B '2' D A R I;
DB2GENERAL : D B '2' G E N E R A L;
DB2GENRL : D B '2' G E N R L;
DB2SQL : D B '2' S Q L;
DBADM : D B A D M;
DBCLOB : D B C L O B;
DBCTRL : D B C T R L;
DBINFO : D B I N F O;
DBMAINT : D B M A I N T;
DEACTIVATE : D E A C T I V A T E;
DEADLOCKS : D E A D L O C K S;
DEALLOCATE : D E A L L O C A T E;
DEBUG : D E B U G;
DEBUGSESSION : D E B U G S E S S I O N;
DEC : D E C;
DEC_ROUND_CEILING : D E C UNDERSCORECHAR R O U N D UNDERSCORECHAR C E I L I N G;
DEC_ROUND_DOWN : D E C UNDERSCORECHAR R O U N D UNDERSCORECHAR D O W N ;
DEC_ROUND_FLOOR : D E C UNDERSCORECHAR R O U N D UNDERSCORECHAR F L O O R;
DEC_ROUND_HALF_DOWN : D E C UNDERSCORECHAR R O U N D UNDERSCORECHAR H A L F UNDERSCORECHAR D O W N;
DEC_ROUND_HALF_EVEN : D E C UNDERSCORECHAR R O U N D UNDERSCORECHAR H A L F UNDERSCORECHAR E V E N;
DEC_ROUND_HALF_UP : D E C UNDERSCORECHAR R O U N D UNDERSCORECHAR H A L F UNDERSCORECHAR U P;
DEC_ROUND_UP : D E C UNDERSCORECHAR R O U N D UNDERSCORECHAR U P;
DECFLOAT : D E C F L O A T;
DECIMAL : D E C I M A L;
DECLARE : D E C L A R E;
DEFAULT : D E F A U L T;
DEFAULTS : D E F A U L T S;
DEFER : D E F E R;
DEFERRABLE : D E F E R R A B L E;
DEFERRED : D E F E R R E D;
DEFINEBIND : D E F I N E B I N D;
DEFINER : D E F I N E R;
DEFINERUN : D E F I N E R U N;
DEFINITION : D E F I N I T I O N;
DEGREE : D E G R E E;
DENSE_RANK: D E N S E UNDERSCORECHAR R A N K;
DEPENDENT : D E P E N D E N T;
DESC : D E S C;
DESCRIBE : D E S C R I B E;
DESCRIPTOR : D E S C R I P T O R;
DETERMINISTIC : D E T E R M I N I S T I C;
DIAGNOSTICS : D I A G N O S T I C S;
DIMENSIONS : D I M E N S I O N S;
DISABLE : D I S A B L E;
DISALLOW : D I S A L L O W;
DISPATCH : D I S P A T C H;
DISPLAY : D I S P L A Y;
DISPLAYDB : D I S P L A Y D B;
DISTINCT : D I S T I N C T;
DISTNCT : D I S T N C T;
DO : D O;
DOMAIN : D O M A I N;
DOUBLE : D O U B L E;
DOUBLEQUOTE : D O U B L E Q U O T E;
DPSEGSZ : D P S E G S Z;
DROP : D R O P;
DROPIN : D R O P I N;
DSNDB04 : D S N D B '0' '4'; //TODO: ask if this is required or it should be just an identifier
DSSIZE: D S S I Z E;
DYNAMIC : D Y N A M I C;
DYNAMICRULES : D Y N A M I C R U L E S;
EACH : E A C H;
EBCDIC : E B C D I C;
EDITPROC: E D I T P R O C;
ELEMENT  : E L E M E N T;
ELIGIBLE : E L I G I B L E;
ELSE : E L S E;
ELSEIF : E L S E I F;
EMPTY : E M P T Y;
ENABLE : E N A B L E;
ENCODING : E N C O D I N G;
ENCRYPTION : E N C R Y P T I O N;
ENDING : E N D I N G;
ENFORCED : E N F O R C E D;
ENVIRONMENT : E N V I R O N M E N T;
ESCAPE : E S C A P E;
EUR : E U R;
EVERY : E V E R Y;
EXACT : E X A C T;
EXCEPT : E X C E P T;
EXCHANGE : E X C H A N G E;
EXCLUDE : E X C L U D E;
EXCLUDING : E X C L U D I N G;
EXCLUSIVE : E X C L U S I V E;
EXISTS : E X I S T S;
EXIT : E X I T;
EXPLAIN : E X P L A I N;
EXTENDED : E X T E N D E D;
EXTENSION : E X T E N S I O N;
EXTRA : E X T R A;
FAILBACK : F A I L B A C K;
FAILURE: F A I L U R E;
FAILURES : F A I L U R E S;
FALSE : F A L S E;
FEDERATED : F E D E R A T E D;
FENCED : F E N C E D;
FETCH : F E T C H;
FIELDPROC: F I E L D P R O C;
FINAL : F I N A L;
FIRST : F I R S T;
FIRST_VALUE: F I R S T UNDERSCORECHAR V A L U E;
FLOAT : F L O A T;
FLUSH : F L U S H;
FOLLOWING : F O L L O W I N G;
FOREIGN : F O R E I G N;
FORMAT : F O R M A T;
FORTRAN : F O R T R A N;
FOUND : F O U N D;
FREEPAGE : F R E E P A G E;
FS : F S;
FULL : F U L L;
FUNCTION : F U N C T I O N;
FUNCTION_LEVEL_10: V NUMBER_1 ZERO_DIGIT R NUMBER_1;
FUNCTION_LEVEL_11: V NUMBER_1 NUMBER_1 R NUMBER_1;
FUNCTION_LEVEL_12: V DIGIT DIGIT R DIGIT M DIGIT DIGIT DIGIT;
G_CHAR : G;
GBPCACHE : G B P C A C H E;
GENERAL : G E N E R A L;
GENERATE : G E N E R A T E;
GENERATED : G E N E R A T E D;
GET_ACCEL_ARCHIVE : G E T UNDERSCORECHAR A C C E L UNDERSCORECHAR A R C H I V E;
GLOBAL : G L O B A L;
GO : G O;
GOTO : G O T O;
GRANT : G R A N T;
GRAPHIC : G R A P H I C;
GROUP : G R O U P;
GROUPING : G R O U P I N G;
HANDLER : H A N D L E R;
HASH : H A S H;
HAVING : H A V I N G;
HIDDENCHAR : H I D D E N;
HIGH : SINGLEQUOTE H I G H SINGLEQUOTE;
HINT: H I N T;
HISTORY : H I S T O R Y;
HOUR : H O U R;
ID : I D;
IDENTITY : I D E N T I T Y;
IF : I F;
IMAGCOPY : I M A G C O P Y;
IMPLICIT_SCHEMA : I M P L I C I T UNDERSCORECHAR S C H E M A;
IMPLICITLY : I M P L I C I T L Y;
IN : I N;
INCLUDE : I N C L U D E;
INCLUDING : I N C L U D I N G;
INCLUSIVE : I N C L U S I V E;
INDEX : I N D E X;
INDEXBP : I N D E X B P;
INDEXES : I N D E X E S;
INDICATOR : I N D I C A T O R;
INHERIT : I N H E R I T;
INITIAL_INSTS : I N I T I A L UNDERSCORECHAR I N S T S;
INITIAL_IOS : I N I T I A L UNDERSCORECHAR I O S;
INITIALLY : I N I T I A L L Y;
INLINE : I N L I N E;
INNER : I N N E R;
INOUT : I N O U T;
INSENSITIVE : I N S E N S I T I V E;
INSTEAD : I N S T E A D;
INSTS_PER_ARGBYTE : I N S T S UNDERSCORECHAR P E R UNDERSCORECHAR A R G B Y T E;
INSTS_PER_INVOC : I N S T S UNDERSCORECHAR P E R UNDERSCORECHAR I N V O C;
INT : I N T;
INTEGER : I N T E G E R;
INTEGRITY : I N T E G R I T Y;
INTERSECT : I N T E RS E C T;
INVALID : I N V A L I D;
INVOKEBIND : I N V O K E B I N D;
INVOKERUN : I N V O K E R U N;
IOS_PER_ARGBYTE : I O S UNDERSCORECHAR P E R UNDERSCORECHAR A R G B Y T E;
IOS_PER_INVOC : I O S UNDERSCORECHAR P E R UNDERSCORECHAR I N V O C;
IS : I S;
ISO : I S O;
ISOLATION : I S O L A T I O N;
ITERATE : I T E R A T E;
JAR : J A R;
JAVA : J A V A;
JIS : J I S;
JOBNAME: J O B N A M E;
JOIN : J O I N;
K_CHAR : K;
KEY : K E Y;
KEYS : K E Y S;
LABELS : L A B E L S;
LAG: L A G;
LANGUAGE : L A N G U A G E;
LANGUAGE_C : C;
LARGE : L A R G E;
LAST_VALUE: L A S T UNDERSCORECHAR V A L U E;
LC_CTYPE : L C UNDERSCORECHAR C T Y P E;
LEAD : L E A D;
LEADING : L E A D I N G;
LEAVE : L E A V E;
LEFT : L E F T;
LIBRARY : L I B R A R Y;
LIKE : L I K E;
LIMIT : L I M I T;
LINKTYPE : L I N K T Y P E;
LITERALS : L I T E R A L S;
LOB : L O B;
LOCAL : L O C A L;
LOCALE : L O C A L E;
LOCATION : L O C A T I O N;
LOCATOR : L O C A T O R;
LOCATORS : L O C A T O R S;
LOCK : L O C K;
LOCKED : L O C K E D;
LOCKMAX : L O C K M A X;
LOCKS : L O C K S;
LOCKSIZE : L O C K S I Z E;
LOGGED : L O G G E D;
LONG : L O N G;
LONGVAR : L O N G V A R;
LOOP : L O O P;
LOW : SINGLEQUOTE L O W SINGLEQUOTE;
LOWER :L O W E R;
QUOTED_NONE : SINGLEQUOTE N O N E SINGLEQUOTE;
M_CHAR : M;
MAINTAINED : M A I N T A I N E D;
MAPPING : M A P P I N G;
MASK : M A S K;
MATCH : M A T C H;
MATCHED : M A T C H E D;
MATERIALIZED : M A T E R I A L I Z E D;
MAX : M A X;
MAXPARTITIONS : M A X P A R T I T I O N S;
MAXROWS : M A X R O W S;
MAXVALUE : M A X V A L U E;
MEMBER : M E M B E R;
MERGE : M E R G E;
MESSAGE_TEXT: M E S S A G E UNDERSCORECHAR T E X T;
MGMTCLAS: M G M T C L A S;
MICROSECOND : M I C R O S E C O N D;
MICROSECONDS : M I C R O S E C O N D S;
MIN: M I N;
MINIVALUE : M I N I V A L U E;
MINUTE : M I N U T E;
MINVALUE : M I N V A L U E;
MIXED : M I X E D;
MODIFIERS : M O D I F I E R S;
MODIFIES :  M O D I F I E S;
MODULE : M O D U L E;
MONITOR1 : M O N I T O R '1';
MONITOR2 : M O N I T O R '2';
MONTHS : M O N T H S;
MORECHAR : M O R E;
MULTIPLIER : M U L T I P L I E R;
NAMED : N A M E D;
NAMES : N A M E S;
NAMESPACE : N A M E S P A C E;
NATIONAL : N A T I O N A L;
NATURAL : N A T U R A L;
NCHAR : N C H A R;
NCNAME: N C N A M E;
NEW : N E W;
NEW_TABLE : N E W UNDERSCORECHAR T A B L E;
NEXTVAL : N E X T V A L;
NICKNAME : N I C K N A M E;
NO : N O;
NOCACHE : N O C A C H E;
NOCYCLE : N O C Y C L E;
NODE : N O M A X V A L U E;
NODEFER : N O D E F E R;
NOMINVALUE : N O M I N V A L U E;
NOORDER : N O O R D E R;
NOT : N O T;
NTH_VALUE: N T H UNDERSCORECHAR V A L U E;
NTILE: N T I L E;
NULL : N U L L;
NULLABLE : N U L L A B L E;
NULLS : N U L L S;
NULTERM : N U L T E R M;
NUMBER : N U M B E R;
NUMERIC : N U M E R I C;
NUMPARTS: N U M P A R T S;
OBID: O B I D;
OBJECT : O B J E C T;
OCTETS : O C T E T S;
OF : O F;
OFF : O F F;
OFFSET : O F F S E T;
OLD : O L D;
OLD_TABLE : O L D UNDERSCORECHAR T A B L E;
OLE : O L E;
OLEDB : O L E D B;
ONCE : O N C E;
ONLINE : O N L I N E;
ONLY : O N L Y;
OPTHINT : O P T H I N T;
OPTIMIZATION : O P T I M I Z A T I O N;
OPTIMIZE : O P T I M I Z E;
OPTION : O P T I O N;
OPTIONAL : O P T I O N A L;
ORDER : O R D E R;
ORDINALITY : O R D I N A L I T Y;
ORGANIZE : O R G A N I Z E;
ORIGINAL : O R I G I N A L;
OUT : O U T;
OUTCOME : O U T C O M E;
OUTER : O U T E R;
OVER : O V E R;
OVERLAPS : O V E R L A P S;
OVERRIDING : O V E R R I D I N G;
OWNERSHIP : O W N E R S H I P;
PACKADM : P A C K A D M;
PACKAGE : P A C K A G E;
PACKAGE_NAME : P A C K A G E UNDERSCORECHAR N A M E;
PACKAGE_SCHEMA : P A C K A G E UNDERSCORECHAR S C H E M A;
PACKAGE_VERSION : P A C K A G E UNDERSCORECHAR V E R S I O N;
PACKAGESET: P A C K A G E S E T;
PAD : P A D;
PADDED : P A D D E D;
PARALLEL : P A R A L L E L;
PARAMETER : P A R A M E T E R;
PART : P A R T;
PARTIAL : P A R T I A L;
PARTITION : P A R T I T I O N;
PARTITIONED: P A R T I T I O N E D;
PARTITIONING : P A R T I T I O N I N G;
PASCAL : P A S C A L;
PASSING : P A S S I N G;
PASSTHRU : P A S S T H R U;
PCTFREE : P C T F R E E;
PENDING : P E N D I N G;
PERCENT_ARGBYTES : P E R C E N T UNDERSCORECHAR A R G B Y T E S;
PERCENT_RANK: P E R C E N T UNDERSCORECHAR R A N K;
PERIOD : P E R I O D;
PERMISSION : P E R M I S S I O N;
PIECESIZE : P I E C E S I Z E;
PIPE : P I P E;
PLAN : P L A N;
PLI : P L I;
PORTION : P O R T I O N;
POSITION : P O S I T I O N;
POSITIONING : P O S I T I O N I N G;
PRECEDING : P R E C E D I N G;
PRECISION : P R E C I S I O N;
PRESERVE : P R E S E R V E;
PREVIOUS: P R E V I O U S;
PRIMARY : P R I M A R Y;
PRIOR : P R I O R;
PRIQTY : P R I Q T Y ;
PRIVELEGES : P R I V E L E G E S;
PRIVILEGES : P R I V I L E G E S;
PROCEDURE : P R O C E D U R E;
PROTOCOL : P R O T O C O L;
PUBLIC : P U B L I C;
QUALIFIER : Q U A L I F I E R;
QUERYNO : Q U E R Y N O;
RANDOM : R A N D O M;
RANGE : R A N G E;
RANK : R A N K;
RATIO_TO_REPORT: R A T I O UNDERSCORECHAR T O UNDERSCORECHAR R E P O R T;
READS : R E A D S;
REAL : R E A L;
RECOMMEND : R E C O M M E N D;
RECOVER : R E C O V E R;
RECOVERDB : R E C O V E R D B;
RECOVERY : R E C O V E R Y;
REF : R E F;
REFERENCE : R E F E R E N C E;
REFERENCES : R E F E R E N C E S;
REFERENCING : R E F E R E N C I N G;
REFRESH : R E F R E S H;
REGENERATE : R E G E N E R A T E;
REGISTERS : R E G I S T E R S;
RELATIVE : R E L A T I V E;
RENAME : R E N A M E;
REOPT : R E O P T;
REORG : R E O R G;
REPAIR : R E P A I R;
REPEAT : R E P E A T;
REPLICATED : R E P L I C A T E D;
REQUIRED : R E Q U I R E D;
RESIDENT : R E S I D E N T;
RESIGNAL : R E S I G N A L;
RESOLUTION : R E S O L U T I O N;
RESOLVE : R E S O L V E;
RESPECT : R E S P E C T;
RESTORE : R E S T O R E;
RESTRICT : R E S T R I C T;
RESULT_SET_LOCATOR : R E S U L T UNDERSCORECHAR S E T UNDERSCORECHAR L O C A T O R;
RETRUN : R E T R U N;
RETURN_STATUS : R E T U R N UNDERSCORECHAR S T A T U S;
RETURNED_SQLSTATE : R E T U R N E D UNDERSCORECHAR S Q L S T A T E;
RETURNS : R E T U R N S;
REUSE : R E U S E;
REVOKE : R E V O K E;
REXX : R E X X;
RIGHT : R I G H T;
ROLLUP : R O L L U P;
ROTATE : R O T A T E;
ROUND_CEILING : R O U N D UNDERSCORECHAR C E I L I N G;
ROUND_DOWN :  R O U N D UNDERSCORECHAR D O W N;
ROUND_FLOOR : R O U N D UNDERSCORECHAR  F L O O R;
ROUND_HALF_DOWN : R O U N D UNDERSCORECHAR H A L F UNDERSCORECHAR D O W N;
ROUND_HALF_EVEN : R O U N D UNDERSCORECHAR H A L F UNDERSCORECHAR E V E N;
ROUND_HALF_UP : R O U N D UNDERSCORECHAR H A L F UNDERSCORECHAR U P;
ROUND_UP : R O U N D UNDERSCORECHAR U P;
ROUNDING : R O U N D I N G;
ROUTINE : R O U T I N E;
ROW : R O W;
ROW_COUNT : R O W UNDERSCORECHAR C O U N T;
ROW_NUMBER: R O W UNDERSCORECHAR N U M B E R;
ROWID : R O W I D;
ROWS : R O W S;
ROWSET : R O W S E T;
RR : R R;
RS : R S;
RULES: R U L E S;
SAVEPOINT : S A V E P O I N T;
SBCS : S B C S;
SCALE : S C A L E;
SCHEMA : S C H E M A;
SCRATCHPAD : S C R A T C H P A D;
SCROLL : S C R O L L;
SEARCH : S E A R C H;
SECOND : S E C O N D;
SECQTY : S E C Q T Y;
SECTION : S E C T I O N;
SECURED : S E C U R E D;
SEGSIZE: S E G S I Z E;
SELECT : S E L E C T;
SELECTIVITY : S E L E C T I V I T Y;
SELF : S E L F;
SENSITIVE : S E N S I T I V E;
SEQUENCE : S E Q U E N C E;
SERIALIZABLE : S E R I A L I Z A B L E;
SERVAUTH : S E R V A U T H;
SERVER_NAME : SERVER UNDERSCORECHAR N A M E;
SESSION_USER : S E S S I O N UNDERSCORECHAR U S E R;
SETS : S E T S;
SHARE : S H A R E;
SHRLEVEL : S H R L E V E L;
SIGNAL : S I G N A  L;
SIMPLE : S I M P L E;
SIZE : S I Z E;
SKIPCHAR : S K I P;
SMALLINT : S M A L L I N T;
SNAPSHOT : S N A P S H O T;
SOME : S O M E;
SOURCE : S O U R C E;
SPACE : S P A C E;
SPECIAL : S P E C I A L;
SPECIFIC : S P E C I F I C;
SQLADM : S Q L A D M;
SQLCA : S Q L C A;
SQLCODE : S Q L C O D E;
SQLD : S Q L D;
SQLDA : S Q L D A;
SQLDABC : S Q L D A B C;
SQLERROR : S Q L E R R O R;
SQLEXCEPTION : S Q L E X C E P T I O N;
SQLID : S Q L I D;
SQLN : S Q L N;
SQLSTATE : S Q L S T A T E;
SQLVAR : S Q L V A R;
SQLWARNING : S Q L W A R N I N G;
STABILIZED : S T A B I L I Z E D;
STACKED : S T A C K E D;
STARTDB : S T A R T D B;
STARTING : S T A R T I N G;
STATEMENT : S T A T E M E N T;
STATEMENTS : S T A T E M E N T S;
STATIC : S T A T I C;
STATISTICS : S T A T I S T I C S;
STATS : S T A T S;
STAY : S T A Y;
STDDEV : S T D D E V;
STMTCACHE : S T M T C A C H E;
STMTID : S T M T I D;
STMTTOKEN : S T M T T O K E N;
STOGROUP : S T O G R O U P;
STOP : S T O P;
STOPALL : S T O P A L L;
STOPDB : S T O P D B;
STORCLAS: S T O R C L A S;
STORED : S T O R E D;
STORES : S T O R E S;
STORIES : S T O R I E S;
STOSPACE : S T O S P A C E;
STRUCTURE : S T R U C T U R E;
STYLE : S T Y L E;
SUB : S U B;
SUBSTR : S U B S T R;
SUBSTRING : S U B S T R I N G;
SUM : S U M;
SUMMARY : S U M M A R Y;
SWITCH : S W I T C H;
SYNONYM : S Y N O N Y M;
SYSADM : S Y S A D M;
SYSCTRL : S Y S C T R L;
SYSDEFLT : S Y S D E F L T;
SYSIBM : S Y S I B M;
SYSOPR : S Y S O P R;
SYSTEM : S Y S T E M;
SYSTEM_TIME : S Y S T E M UNDERSCORECHAR T I M E;
SYSTEM_USER : S Y S T E M UNDERSCORECHAR U S E R;
TABLE : T A B L E;
TABLE_NAME : T A B L E UNDERSCORECHAR N A M E;
TABLESPACE : T A B L E S P A C E;
TABLESPACES : T A B L E S P A C E S;
TEMPORAL: T E M P O R A L;
TEMPORARY : T E M P O R A R Y;
THEN : T H E N;
THREADSAFE : T H R E A D S A F E;
TIMESTAMP : T I M E S T A M P;
TIMEZONE : T I M E Z O N E;
TIMEZONE_HOUR : T I M E Z O N E UNDERSCORECHAR H O U R;
TIMEZONE_MINUTE : T I M E Z O N E UNDERSCORECHAR M I N U T E;
TRACKMOD : T R A C K M O D;
TRAILING : T R A I L I N G;
TRANSFER : T R A N S F E R;
TRANSLATE : T R A N S L A T E;
TRANSLATION : T R A N S L A T I O N;
TREAT : T R E A T;
TRIGGERS : T R I G G E R S;
TRIM : T R I M;
TRUE : T R U E;
TRUNCATE : T R U N C A T E;
TRUSTED : T R U S T E D;
TYPES: T Y P E S;
UNBOUNDED : U N B O U N D E D;
UNDER : U N D E R;
UNDO : U N D O;
UNICODE : U N I C O D E;
UNION : U N I O N;
UNIQUE : U N I Q U E;
UNKNOWN : U N K N O W N;
UNNEST : U N N E S T;
UNPACK : U N P A C K;
UPON : U P O N;
UPPER : U P P E R;
UR : U R;
USA : U S A;
USAGE : U S A G E;
USE : U S E;
USER : U S E R;
V1 : V '1';
VALIDATE : V A L I D A T E;
VALIDPROC: V A L I D P R O C;
VALUES : V A L U E S;
VARBINARY : V A R B I N A R Y;
VARCHAR : V A R C H A R;
VARGRAPHIC : V A R G R A P H I C;
VARIABLE : V A R I A B L E;
VARIANCE: V A R I A N C E;
VARIANT : V A R I A N T;
VARYING : V A R Y I N G;
VCAT : V C A T;
VERSION : V E R S I O N;
VERSIONING : V E R S I O N I N G;
VERSIONS : V E R S I O N S;
VIEW : V I E W;
VOLATILE : V O L A T I L E;
VOLUMES : V O L U M E S;
WAITFORDATA : W A I T F O R D A T A;
WHEN : W H E N;
WHENEVER : W H E N E V E R;
WHERE : W H E R E;
WHILE : W H I L E;
WITH : W I T H;
WITHOUT : W I T H O U T;
WLM : W L M;
WORK : W O R K;
WORKFILE : W O R K F I L E;
WRAPPED : W R A P P E D;
WRAPPER : W R A P P E R;
WRKSTNNAME: W R K S T N N A M E;
XML : X M L;
XMLCAST: XML C A S T;
XMLNAMESPACES: X M L N A M E S P A C E S;
XMLPATTERN: X M L P A T T E R N;
XMLSCHEMA: X M L S C H E M A;
XMLTABLE : X M L T A B L E;
YEARS : Y E A R S;
YES : Y E S;
ZONE : Z O N E;

// Constructors symbols
GRAPHIC_CONSTANT : GRAPHICUNICODE | GRAHICCHAR;
ATCHAR :             '@';
DOUBLESLASHCHAR : '//';
LSQUAREBRACKET :     '[';
RSQUAREBRACKET :     ']';
REVERSEQUOTECHAR :   '`';
SEMICOLONCHAR :      ';';
UNDERSCORECHAR :     '_';
PIPECHAR : '||';
QUESTIONMARK : '?';
PERCENT : '%';
SELECT_ALL : '.*';
TIMESTAMPLITERAL: DIGIT DIGIT '.' DIGIT DIGIT '.' DIGIT DIGIT | // hh.mm.ss;
                  DIGIT DIGIT '.' DIGIT DIGIT (A M | P M) | //hh:mm AM /PM
                  DIGIT DIGIT '.' DIGIT DIGIT ':' DIGIT DIGIT |// hh.mm:ss
                  DIGIT DIGIT ':' DIGIT DIGIT ':' DIGIT DIGIT '.'  DIGIT DIGIT DIGIT DIGIT DIGIT DIGIT;//hh:mm:ss.nnnnnn
DATELITERAL: '\'' (DIGIT DIGIT DIGIT DIGIT '-' DIGIT DIGIT '-' DIGIT DIGIT | //yyyy-mm-dd
                   DIGIT DIGIT '/' DIGIT DIGIT '/' DIGIT DIGIT DIGIT DIGIT | //mm/dd/yyyy
                   DIGIT DIGIT '.' DIGIT DIGIT '.' DIGIT DIGIT DIGIT DIGIT |//dd.mm.yyyy
                   DIGIT DIGIT '-' DIGIT DIGIT '-' DIGIT DIGIT DIGIT DIGIT) TIMESTAMPLITERAL? '\'';//yyyy-mm-dd

fragment BXNUMBER :
	B X '"' [0-9A-F]+ '"'
	| B X '\'' [0-9A-F]+ '\''
;

fragment GRAPHICUNICODE :
	U X '"' [0-9A-F]+ '"'
	| U X '\'' [0-9A-F]+ '\''
;

fragment GRAHICCHAR :
	G X '"' [0-9A-F]+ '"'
	| G X '\'' [0-9A-F]+ '\''
;
