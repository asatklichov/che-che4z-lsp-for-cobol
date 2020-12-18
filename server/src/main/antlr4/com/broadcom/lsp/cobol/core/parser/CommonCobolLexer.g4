lexer grammar CommonCobolLexer;

@lexer::members {
    boolean sqlFlag = false;
}

ABCODE : A B C O D E;
ABDUMP : A B D U M P;
ABEND : A B E N D;
ABORT : A B O R T;
ABPROGRAM : A B P R O G R A M;
ABSTIME : A B S T I M E;
ACCUM : A C C U M;
ACEE : A C E E;
ACQACTIVITY : A C Q A C T I V I T Y;
ACQPROCESS : A C Q P R O C E S S;
ACQUACTIVITY : A C Q U A C T I V I T Y;
ACTION : A C T I O N;
ACTIVITY : A C T I V I T Y;
ACTIVITYID : A C T I V I T Y I D;
ACTPARTN : A C T P A R T N;
ADD : A D D;
ADDRESS : A D D R E S S;
AFTER : A F T E R;
AID : A I D;
ALARM : A L A R M;
ALLOCATE : A L L O C A T E;
ALTER : A L T E R;
ALTSCRNHT : A L T S C R N H T;
ALTSCRNWD : A L T S C R N W D;
AND : A N D;
ANYKEY : A N Y K E Y;
APLKYBD : A P L K Y B D;
APLTEXT : A P L T E X T;
APPLID : A P P L I D;
AS : A S;
ASA : A S A;
ASIS : A S I S;
ASKTIME : A S K T I M E;
ASRAINTRPT : A S R A I N T R P T;
ASRAKEY : A S R A K E Y;
ASRAPSW : A S R A P S W;
ASRAREGS : A S R A R E G S;
ASRASPC : A S R A S P C;
ASRASTG : A S R A S T G;
ASSIGN : A S S I G N;
ASYNCHRONOUS : A S Y N C H R O N O U S;
AT : A T;
ATTACH : A T T A C H;
ATTACHID : A T T A C H I D;
ATTRIBUTES : A T T R I B U T E S;
AUTHENTICATE : A U T H E N T I C A T E;
AUTOPAGE : A U T O P A G E;
AUXILIARY : A U X I L I A R Y;
BASE64 : B A S E '6' '4';
BASICAUTH : B A S I C A U T H;
BELOW : B E L O W;
BIF : B I F;
BINARY : B I N A R Y;
BIT : B I T;
BODYCHARSET : B O D Y C H A R S E T;
BOOKMARK : B O O K M A R K;
BRDATA : B R D A T A;
BRDATALENGTH : B R D A T A L E N G T H;
BREXIT : B R E X I T;
BRIDGE : B R I D G E;
BROWSETOKEN : B R O W S E T O K E N;
BTRANS : B T R A N S;
BUFFER : B U F F E R;
BUILD : B U I L D;
BURGEABILITY : B U R G E A B I L I T Y;
CADDRLENGTH : C A D D R L E N G T H;
CANCEL : C A N C E L;
CARD : C A R D;
CBUFF : C B U F F;
CCSID : C C S I D;
CERTIFICATE : C E R T I F I C A T E;
CHANGE : C H A N G E;
CHANGETIME : C H A N G E T I M E;
CHANNEL : C H A N N E L;
CHAR : C H A R;
CHARACTERSET : C H A R A C T E R S E T;
CHECK : C H E C K;
CHUNKEND : C H U N K E N D;
CHUNKING : C H U N K I N G;
CHUNKNO : C H U N K N O;
CHUNKYES : C H U N K Y E S;
CICSDATAKEY : C I C S D A T A K E Y;
CIPHERS : C I P H E R S;
CLASS : C L A S S;
CLEAR : C L E A R;
CLICONVERT : C L I C O N V E R T;
CLIENT : C L I E N T;
CLIENTADDR : C L I E N T A D D R;
CLIENTADDRNU : C L I E N T A D D R N U;
CLIENTCONV : C L I E N T C O N V;
CLIENTNAME : C L I E N T N A M E;
CLNTADDR6NU : C L N T A D D R '6' N U;
CLNTIPFAMILY : C L N T I P F A M I L Y;
CLOSE : C L O S E;
CLOSESTATUS : C L O S E S T A T U S;
CLRPARTN : C L R P A R T N;
CMDSEC : C M D S E C;
CNAMELENGTH : C N A M E L E N G T H;
CNOTCOMPL : C N O T C O M P L;
CODEPAGE : C O D E P A G E;
COLOR : C O L O R;
COMMAREA : C O M M A R E A;
COMMONNAME : C O M M O N N A M E;
COMMONNAMLEN : C O M M O N N A M L E N;
COMPAREMAX : C O M P A R E M A X;
COMPAREMIN : C O M P A R E M I N;
COMPLETE : C O M P L E T E;
COMPOSITE : C O M P O S I T E;
COMPSTATUS : C O M P S T A T U S;
CONDITION : C O N D I T I O N;
CONFIRM : C O N F I R M;
CONFIRMATION : C O N F I R M A T I O N;
CONNECT : C O N N E C T;
CONSISTENT : C O N S I S T E N T;
CONSOLE : C O N S O L E;
CONTAINER : C O N T A I N E R;
CONTEXTTYPE : C O N T E X T T Y P E;
CONTROL : C O N T R O L;
CONVDATA : C O N V D A T A;
CONVERSE : C O N V E R S E;
CONVERTST : C O N V E R T S T;
CONVERTTIME : C O N V E R T T I M E;
CONVID : C O N V I D;
COPY : C O P Y;
COUNTER : C O U N T E R;
COUNTRY : C O U N T R Y;
COUNTRYLEN : C O U N T R Y L E N;
CREATE : C R E A T E;
CRITICAL : C R I T I C A L;
CTLCHAR : C T L C H A R;
CURRENT : C U R R E N T;
CWA : C W A;
CWALENG : C W A L E N G;
DATA : D A T A;
DATA1 : D A T A '1';
DATA2 : D A T A '2';
DATALENGTH : D A T A L E N G T H;
DATALENTH : D A T A L E N T H;
DATAONLY : D A T A O N L Y;
DATAPOINTER : D A T A P O I N T E R;
DATASET : D A T A S E T;
DATASTR : D A T A S T R;
DATATOXML : D A T A T O X M L;
DATATYPE : D A T A T Y P E;
DATCONTAINER : D A T C O N T A I N E R;
DATE : D A T E;
DATEFORM : D A T E F O R M;
DATESEP : D A T E S E P;
DATESTRING : D A T E S T R I N G;
DAYCOUNT : D A Y C O U N T;
DAYOFMONTH : D A Y O F M O N T H;
DAYOFWEEK : D A Y O F W E E K;
DAYOFYEAR : D A Y O F Y E A R;
DAYS : D A Y S;
DAYSLEFT : D A Y S L E F T;
DCOUNTER : D C O U N T E R;
DDMMYY : D D M M Y Y;
DDMMYYYY : D D M M Y Y Y Y;
DEBKEY : D E B K E Y;
DEBREC : D E B R E C;
DEEDIT : D E E D I T;
DEFINE : D E F I N E;
DEFRESP : D E F R E S P;
DEFSCRNHT : D E F S C R N H T;
DEFSCRNWD : D E F S C R N W D;
DELAY : D E L A Y;
DELETE : D E L E T E;
DELETEQ : D E L E T E Q;
DELIMITER : D E L I M I T E R;
DEQ : D E Q;
DESTCOUNT : D E S T C O U N T;
DESTID : D E S T I D;
DESTIDLENG : D E S T I D L E N G;
DETAIL : D E T A I L;
DETAILLENGTH : D E T A I L L E N G T H;
DIGEST : D I G E S T;
DIGESTTYPE : D I G E S T T Y P E;
DISCONNECT : D I S C O N N E C T;
DOCDELETE : D O C D E L E T E;
DOCSIZE : D O C S I Z E;
DOCSTATUS : D O C S T A T U S;
DOCTOKEN : D O C T O K E N;
DOCUMENT : D O C U M E N T;
DS3270 : D S '3' '2' '7' '0';
DSSCS : D S S C S;
DUMP : D U M P;
DUMPCODE : D U M P C O D E;
DUMPID : D U M P I D;
ECADDR : E C A D D R;
ECBLIST : E C B L I S T;
EIB : E I B;
ELEMNAME : E L E M N A M E;
ELEMNAMELEN : E L E M N A M E L E N;
ELEMNS : E L E M N S;
ELEMNSLEN : E L E M N S L E N;
END : E N D;
ENDACTIVITY : E N D A C T I V I T Y;
ENDBR : E N D B R;
ENDBROWSE : E N D B R O W S E;
ENDFILE : E N D F I L E;
ENDOUTPUT : E N D O U T P U T;
ENQ : E N Q;
ENTER : E N T E R;
ENTRY : E N T R Y;
ENTRYNAME : E N T R Y N A M E;
EOC : E O C;
EODS : E O D S;
EPRFIELD : E P R F I E L D;
EPRFROM : E P R F R O M;
EPRINTO : E P R I N T O;
EPRLENGTH : E P R L E N G T H;
EPRSET : E P R S E T;
EPRTYPE : E P R T Y P E;
EQUAL : E Q U A L;
ERASE : E R A S E;
ERASEAUP : E R A S E A U P;
ERROR : E R R O R;
ERRTERM : E R R T E R M;
ESMREASON : E S M R E A S O N;
ESMRESP : E S M R E S P;
EVENT : E V E N T;
EVENTTYPE : E V E N T T Y P E;
EVENTUAL : E V E N T U A L;
EWASUPP : E W A S U P P;
EXCEPTION : E X C E P T I O N;
EXPECT : E X P E C T;
EXPIRYTIME : E X P I R Y T I M E;
EXTDS : E X T D S;
EXTERNAL : E X T E R N A L;
EXTRACT : E X T R A C T;
FACILITY : F A C I L I T Y;
FACILITYTOKN : F A C I L I T Y T O K N;
FAULTACTLEN : F A U L T A C T L E N;
FAULTACTOR : F A U L T A C T O R;
FAULTCODE : F A U L T C O D E;
FAULTCODELEN : F A U L T C O D E L E N;
FAULTCODESTR : F A U L T C O D E S T R;
FAULTSTRING : F A U L T S T R I N G;
FAULTSTRLEN : F A U L T S T R L E N;
FCI : F C I;
FCT : F C T;
FIELD : F I E L D;
FILE : F I L E;
FIRESTATUS : F I R E S T A T U S;
FLENGTH : F L E N G T H;
FMH : F M H;
FMHPARM : F M H P A R M;
FOR : F O R;
FORCE : F O R C E;
FORMATTIME : F O R M A T T I M E;
FORMFEED : F O R M F E E D;
FORMFIELD : F O R M F I E L D;
FREE : F R E E;
FREEKB : F R E E K B;
FREEMAIN : F R E E M A I N;
FROM : F R O M;
FROMACTIVITY : F R O M A C T I V I T Y;
FROMCCSID : F R O M C C S I D;
FROMCHANNEL : F R O M C H A N N E L;
FROMCODEPAGE : F R O M C O D E P A G E;
FROMDOC : F R O M D O C;
FROMFLENGTH : F R O M F L E N G T H;
FROMLENGTH : F R O M L E N G T H;
FROMPROCESS : F R O M P R O C E S S;
FRSET : F R S E T;
FULLDATE : F U L L D A T E;
GCHARS : G C H A R S;
GCODES : G C O D E S;
GDS : G D S;
GENERIC : G E N E R I C;
GET : G E T;
GETMAIN : G E T M A I N;
GETNEXT : G E T N E X T;
GMMI : G M M I;
GROUPID : G R O U P I D;
GTEC : G T E C;
GTEQ : G T E Q;
HANDLE : H A N D L E;
HEAD : H E A D;
HEADER : H E A D E R;
HEX : H E X;
HILIGHT : H I L I G H T;
HOLD : H O L D;
HONEOM : H O N E O M;
HOST : H O S T;
HOSTCODEPAGE : H O S T C O D E P A G E;
HOSTLENGTH : H O S T L E N G T H;
HOSTTYPE : H O S T T Y P E;
HOURS : H O U R S;
HTTPHEADER : H T T P H E A D E R;
HTTPMETHOD : H T T P M E T H O D;
HTTPRNUM : H T T P R N U M;
HTTPVERSION : H T T P V E R S I O N;
HTTPVNUM : H T T P V N U M;
IGNORE : I G N O R E;
IMMEDIATE : I M M E D I A T E;
INCREMENT : I N C R E M E N T;
INITIMG : I N I T I M G;
INITPARM : I N I T P A R M;
INITPARMLEN : I N I T P A R M L E N;
INPARTN : I N P A R T N;
INPUT : I N P U T;
INPUTEVENT : I N P U T E V E N T;
INPUTMSG : I N P U T M S G;
INPUTMSGLEN : I N P U T M S G L E N;
INQUIRE : I N Q U I R E;
INSERT : I N S E R T;
INTERVAL : I N T E R V A L;
INTO : I N T O;
INTOCCSID : I N T O C C S I D;
INTOCODEPAGE : I N T O C O D E P A G E;
INVALIDCOUNT : I N V A L I D C O U N T;
INVITE : I N V I T E;
INVMPSZ : I N V M P S Z;
INVOKE : I N V O K E;
INVOKINGPROG : I N V O K I N G P R O G;
INVPARTN: I N V P A R T N;
INVREQ: I N V R E Q;
ISSUE : I S S U E;
ISSUER : I S S U E R;
ITEM : I T E M;
IUTYPE : I U T Y P E;
JOURNALNAME : J O U R N A L N A M E;
JTYPEID : J T Y P E I D;
JUSFIRST : J U S F I R S T;
JUSLAST : J U S L A S T;
JUSTIFY : J U S T I F Y;
KATAKANA : K A T A K A N A;
KEEP : K E E P;
KEYLENGTH : K E Y L E N G T H;
KEYNUMBER : K E Y N U M B E R;
L40 : L '4' '0';
L64 : L '6' '4';
L80 : L '8' '0';
LABEL : L A B E L;
LANGINUSE : L A N G I N U S E;
LANGUAGECODE : L A N G U A G E C O D E;
LAST : L A S T;
LASTUSETIME : L A S T U S E T I M E;
LDC : L D C;
LDCMNEM : L D C M N E M;
LDCNUM : L D C N U M;
LEAVEKB : L E A V E K B;
LENGTH : L E N G T H;
LENGTHLIST : L E N G T H L I S T;
LEVEL : L E V E L;
LIGHTPEN : L I G H T P E N;
LINE : L I N E;
LINEADDR : L I N E A D D R;
LINK : L I N K;
LIST : L I S T;
LISTLENGTH : L I S T L E N G T H;
LLID : L L I D;
LOAD : L O A D;
LOCALITY : L O C A L I T Y;
LOCALITYLEN : L O C A L I T Y L E N;
LOGMESSAGE : L O G M E S S A G E;
LOGMODE : L O G M O D E;
LOGONLOGMODE : L O G O N L O G M O D E;
LOGONMSG : L O G O N M S G;
LUNAME : L U N A M E;
MAIN : M A I N;
MAP : M A P;
MAPCOLUMN : M A P C O L U M N;
MAPFAIL: M A P F A I L;
MAPHEIGHT : M A P H E I G H T;
MAPLINE : M A P L I N E;
MAPONLY : M A P O N L Y;
MAPPED : M A P P E D;
MAPPINGDEV : M A P P I N G D E V;
MAPSET : M A P S E T;
MAPWIDTH : M A P W I D T H;
MASSINSERT : M A S S I N S E R T;
MAXDATALEN : M A X D A T A L E N;
MAXFLENGTH : M A X F L E N G T H;
MAXIMUM : M A X I M U M;
MAXLENGTH : M A X L E N G T H;
MAXLIFETIME : M A X L I F E T I M E;
MAXPROCLEN : M A X P R O C L E N;
MCC : M C C;
MEDIATYPE : M E D I A T Y P E;
MESSAGE : M E S S A G E;
MESSAGEID : M E S S A G E I D;
METADATA : M E T A D A T A;
METADATALEN : M E T A D A T A L E N;
METHOD : M E T H O D;
METHODLENGTH : M E T H O D L E N G T H;
MILLISECONDS : M I L L I S E C O N D S;
MINIMUM : M I N I M U M;
MINUTES : M I N U T E S;
MMDDYY : M M D D Y Y;
MMDDYYYY : M M D D Y Y Y Y;
MODE : M O D E;
MODENAME : M O D E N A M E;
MONITOR : M O N I T O R;
MONTH : M O N T H;
MONTHOFYEAR : M O N T H O F Y E A R;
MOVE : M O V E;
MSR : M S R;
MSRCONTROL : M S R C O N T R O L;
NAME : N A M E;
NAMELENGTH : N A M E L E N G T H;
NATLANG : N A T L A N G;
NATLANGINUSE : N A T L A N G I N U S E;
NETNAME : N E T N A M E;
NEWPASSWORD : N E W P A S S W O R D;
NEWPHRASE : N E W P H R A S E;
NEWPHRASELEN : N E W P H R A S E L E N;
NEXT : N E X T;
NEXTTRANSID : N E X T T R A N S I D;
NLEOM : N L E O M;
NOAUTOPAGE : N O A U T O P A G E;
NOCC : N O C C;
NOCHECK : N O C H E C K;
NOCLICONVERT : N O C L I C O N V E R T;
NOCLOSE : N O C L O S E;
NODATA : N O D A T A;
NODE : N O D E;
NODOCDELETE : N O D O C D E L E T E;
NODUMP : N O D U M P;
NOEDIT : N O E D I T;
NOFLUSH : N O F L U S H;
NOHANDLE : N O H A N D L E -> channel(HIDDEN);
NOINCONVERT : N O I N C O N V E R T;
NONE : N O N E;
NOOUTCONERT : N O O U T C O N E R T;
NOQUEUE : N O Q U E U E;
NOQUIESCE : N O Q U I E S C E;
NOSRVCONVERT : N O S R V C O N V E R T;
NOSUSPEND : N O S U S P E N D;
NOTE : N O T E;
NOTPURGEABLE : N O T P U R G E A B L E;
NOTRUNCATE : N O T R U N C A T E;
NOWAIT : N O W A I T;
NSCONTAINER : N S C O N T A I N E R;
NUMCIPHERS : N U M C I P H E R S;
NUMEVENTS : N U M E V E N T S;
NUMITEMS : N U M I T E M S;
NUMREC : N U M R E C;
NUMROUTES : N U M R O U T E S;
NUMSEGMENTS : N U M S E G M E N T S;
NUMTAB : N U M T A B;
OIDCARD : O I D C A R D;
ON : O N;
OPCLASS : O P C L A S S;
OPEN : O P E N;
OPERATION : O P E R A T I O N;
OPERATOR : O P E R A T O R;
OPERID : O P E R I D;
OPERKEYS : O P E R K E Y S;
OPERPURGE : O P E R P U R G E;
OPID : O P I D;
OPSECURITY : O P S E C U R I T Y;
OPTIONS : O P T I O N S;
OR : O R;
ORGABCODE : O R G A B C O D E;
ORGANIZATION : O R G A N I Z A T I O N;
ORGANIZATLEN : O R G A N I Z A T L E N;
ORGUNIT : O R G U N I T;
ORGUNITLEN : O R G U N I T L E N;
OUTDESCR : O U T D E S C R;
OUTLINE : O U T L I N E;
OUTPARTN : O U T P A R T N;
OUTPUT : O U T P U T;
OWNER : O W N E R;
PA : P A;
PA1: P A '1';
PA2: P A '2';
PA3: P A '3';
PAGE : P A G E;
PAGENUM : P A G E N U M;
PAGING : P A G I N G;
PARSE : P A R S E;
PARTN: P A R T N;
PARTNER : P A R T N E R;
PARTNFAIL: P A R T N F A I L;
PARTNPAGE : P A R T N P A G E;
PARTNS : P A R T N S;
PARTNSET : P A R T N S E T;
PASS : P A S S;
PASSBK : P A S S B K;
PASSWORD : P A S S W O R D;
PASSWORDLEN : P A S S W O R D L E N;
PATH : P A T H;
PATHLENGTH : P A T H L E N G T H;
PCT : P C T;
PF : P F;
PF1 : P F '1';
PF10 : P F '10';
PF11 : P F '11';
PF12 : P F '12';
PF13 : P F '13';
PF14 : P F '14';
PF15 : P F '15';
PF16 : P F '16';
PF17 : P F '17';
PF18 : P F '18';
PF19 : P F '19';
PF2 : P F '2';
PF20 : P F '20';
PF21 : P F '21';
PF22 : P F '22';
PF23 : P F '23';
PF24 : P F '24';
PF3 : P F '3';
PF4 : P F '4';
PF5 : P F '5';
PF6 : P F '6';
PF7 : P F '7';
PF8 : P F '8';
PF9 : P F '9';
PFXLENG : P F X L E N G;
PHRASE : P H R A S E;
PHRASELEN : P H R A S E L E N;
PIPLENGTH : P I P L E N G T H;
PIPLIST : P I P L I S T;
POINT : P O I N T;
POOL : P O O L;
POP : P O P;
PORTNUMBER : P O R T N U M B E R;
PORTNUMNU : P O R T N U M N U;
POST : P O S T;
PPT : P P T;
PREDICATE : P R E D I C A T E;
PREFIX : P R E F I X;
PREPARE : P R E P A R E;
PRINCONVID : P R I N C O N V I D;
PRINSYSID : P R I N S Y S I D;
PRINT : P R I N T;
PRIORITY : P R I O R I T Y;
PRIVACY : P R I V A C Y;
PROCESS : P R O C E S S;
PROCESSTYPE : P R O C E S S T Y P E;
PROCLENGTH : P R O C L E N G T H;
PROCNAME : P R O C N A M E;
PROFILE : P R O F I L E;
PROGRAM : P R O G R A M;
PROTECT : P R O T E C T;
PS: P S;
PUNCH : P U N C H;
PURGE : P U R G E;
PURGEABLE : P U R G E A B L E;
PUSH : P U S H;
PUT : P U T;
QNAME : Q N A M E;
QUERY : Q U E R Y;
QUERYPARM : Q U E R Y P A R M;
QUERYSTRING : Q U E R Y S T R I N G;
QUERYSTRLEN : Q U E R Y S T R L E N;
QUEUE : Q U E U E;
RBA : R B A;
RBN : R B N;
RDATT : R D A T T;
READ : R E A D;
READNEXT : R E A D N E X T;
READPREV : R E A D P R E V;
READQ : R E A D Q;
REATTACH : R E A T T A C H;
RECEIVE : R E C E I V E;
RECEIVER : R E C E I V E R;
RECFM : R E C F M;
RECORD : R E C O R D;
RECORDLEN : R E C O R D L E N;
RECORDLENGTH : R E C O R D L E N G T H;
REDUCE : R E D U C E;
REFPARMS : R E F P A R M S;
REFPARMSLEN : R E F P A R M S L E N;
RELATESINDEX : R E L A T E S I N D E X;
RELATESTYPE : R E L A T E S T Y P E;
RELATESURI : R E L A T E S U R I;
RELEASE : R E L E A S E;
REMOVE : R E M O V E;
REPEATABLE : R E P E A T A B L E;
REPETABLE : R E P E T A B L E;
REPLACE : R E P L A C E;
REPLY : R E P L Y;
REPLYLENGTH : R E P L Y L E N G T H;
REQID : R E Q I D;
REQUESTTYPE : R E Q U E S T T Y P E;
RESCLASS : R E S C L A S S;
RESET : R E S E T;
RESETBR : R E S E T B R;
RESID : R E S I D;
RESIDLENGTH : R E S I D L E N G T H;
RESOURCE : R E S O U R C E;
RESP : R E S P;
RESP2 : R E S P '2';
RESSEC : R E S S E C;
RESTART : R E S T A R T;
RESTYPE : R E S T Y P E;
RESULT : R E S U L T;
RESUME : R E S U M E;
RETAIN : R E T A I N;
RETCODE : R E T C O D E;
RETCORD : R E T C O R D;
RETRIECE : R E T R I E C E;
RETRIEVE : R E T R I E V E;
RETURN : R E T U R N;
RETURNPROG : R E T U R N P R O G;
REWIND : R E W I N D;
REWRITE : R E W R I T E;
RIDFLD : R I D F L D;
ROLE : R O L E;
ROLELENGTH : R O L E L E N G T H;
ROLLBACK : R O L L B A C K;
ROUTE : R O U T E;
ROUTECODES : R O U T E C O D E S;
RPROCESS : R P R O C E S S;
RRESOURCE : R R E S O U R C E;
RRN : R R N;
RTERMID : R T E R M I D;
RTRANSID : R T R A N S I D;
RUN : R U N;
SADDRLENGTH : S A D D R L E N G T H;
SCHEME : S C H E M E;
SCHEMENAME : S C H E M E N A M E;
SCOPE : S C O P E;
SCOPELEN : S C O P E L E N;
SCRNHT : S C R N H T;
SCRNWD : S C R N W D;
SECONDS : S E C O N D S;
SECURITY : S E C U R I T Y;
SEGMENTLIST : S E G M E N T L I S T;
SEND : S E N D;
SENDER : S E N D E R;
SERIALNUM : S E R I A L N U M;
SERIALNUMLEN : S E R I A L N U M L E N;
SERVER : S E R V E R;
SERVERADDR : S E R V E R A D D R;
SERVERADDRNU : S E R V E R A D D R N U;
SERVERCONV : S E R V E R C O N V;
SERVERNAME : S E R V E R N A M E;
SERVICE : S E R V I C E;
SESSION : S E S S I O N;
SESSTOKEN : S E S S T O K E N;
SET : S E T;
SHARED : S H A R E D;
SIGDATA : S I G D A T A;
SIGNAL : S I G N A L;
SIGNOFF : S I G N O F F;
SIGNON : S I G N O N;
SIT : S I T;
SNAMELENGTH : S N A M E L E N G T H;
SOAPFAULT : S O A P F A U L T;
SOSI : S O S I;
SPOOLCLOSE : S P O O L C L O S E;
SPOOLOPEN : S P O O L O P E N;
SPOOLREAD : S P O O L R E A D;
SPOOLWRITE : S P O O L W R I T E;
SRVCONVERT : S R V C O N V E R T;
SRVRADDR6NU : S R V R A D D R '6' N U;
SRVRIPFAMILY : S R V R I P F A M I L Y;
SSLTYPE : S S L T Y P E;
START : S T A R T;
STARTBR : S T A R T B R;
STARTBROWSE : S T A R T B R O W S E;
STARTCODE : S T A R T C O D E;
STATE : S T A T E;
STATELEN : S T A T E L E N;
STATIONID : S T A T I O N I D;
STATUS : S T A T U S;
STATUSCODE : S T A T U S C O D E;
STATUSLEN : S T A T U S L E N;
STATUSTEXT : S T A T U S T E X T;
STORAGE : S T O R A G E;
STRFIELD : S T R F I E L D;
STRINGFORMAT : S T R I N G F O R M A T;
SUBADDR : S U B A D D R;
SUBCODELEN : S U B C O D E L E N;
SUBCODESTR : S U B C O D E S T R;
SUBEVENT : S U B E V E N T;
SUBEVENT1 : S U B E V E N T '1';
SUBEVENT2 : S U B E V E N T '2';
SUBEVENT3 : S U B E V E N T '3';
SUBEVENT4 : S U B E V E N T '4';
SUBEVENT5 : S U B E V E N T '5';
SUBEVENT6 : S U B E V E N T '6';
SUBEVENT7 : S U B E V E N T '7';
SUBEVENT8 : S U B E V E N T '8';
SUSPEND : S U S P E N D;
SUSPSTATUS : S U S P S T A T U S;
SYMBOL : S Y M B O L;
SYMBOLLIST : S Y M B O L L I S T;
SYNCHRONOUS : S Y N C H R O N O U S;
SYNCLEVEL : S Y N C L E V E L;
SYNCONRETURN : S Y N C O N R E T U R N;
SYNCPOINT : S Y N C P O I N T;
SYSID : S Y S I D;
TABLES : T A B L E S;
TASK : T A S K;
TASKPRIORITY : T A S K P R I O R I T Y;
TCPIP : T C P I P;
TCPIPSERVICE : T C P I P S E R V I C E;
TCT : T C T;
TCTUA : T C T U A;
TCTUALENG : T C T U A L E N G;
TD : T D;
TELLERID : T E L L E R I D;
TEMPLATE : T E M P L A T E;
TERMCODE : T E R M C O D E;
TERMID : T E R M I D;
TERMINAL : T E R M I N A L;
TERMPRIORITY : T E R M P R I O R I T Y;
TEST : T E S T;
TEXT : T E X T;
TEXTKYBD : T E X T K Y B D;
TEXTLENGTH : T E X T L E N G T H;
TEXTPRINT : T E X T P R I N T;
TIME : T I M E;
TIMEOUT : T I M E O U T;
TIMER : T I M E R;
TIMESEP : T I M E S E P;
TITLE : T I T L E;
TO : T O;
TOACTIVITY : T O A C T I V I T Y;
TOCHANNEL : T O C H A N N E L;
TOCONTAINER : T O C O N T A I N E R;
TOFLENGTH : T O F L E N G T H;
TOKEN : T O K E N;
TOLENGTH : T O L E N G T H;
TOPROCESS : T O P R O C E S S;
TRACE : T R A C E;
TRACENUM : T R A C E N U M;
TRAILER : T R A I L E R;
TRANPRIORITY : T R A N P R I O R I T Y;
TRANSACTION : T R A N S A C T I O N;
TRANSFORM : T R A N S F O R M;
TRANSID : T R A N S I D;
TRIGGER : T R I G G E R;
TRT : T R T;
TS : T S;
TWA : T W A;
TWALENG : T W A L E N G;
TYPE : T Y P E;
TYPENAME : T Y P E N A M E;
TYPENAMELEN : T Y P E N A M E L E N;
TYPENS : T Y P E N S;
TYPENSLEN : T Y P E N S L E N;
UNATTEND : U N A T T E N D;
UNCOMMITTED : U N C O M M I T T E D;
UNESCAPED : U N E S C A P E D;
UNEXPIN : U N E X P I N;
UNLOCK : U N L O C K;
UNTIL : U N T I L;
SQL : S Q L;
EXEC_SQL: EXEC WS SQL {sqlFlag = true;};
END_EXEC : E N D MINUSCHAR E X E C {sqlFlag = false;};
EXEC : E X E C;
EXECUTE : E X E C U T E;
UOW : U O W;
UPDATE : U P D A T E;
URI : U R I;
URIMAP : U R I M A P;
URL : U R L;
URLLENGTH : U R L L E N G T H;
USERDATAKEY : U S E R D A T A K E Y;
USERID : U S E R I D;
USERNAME : U S E R N A M E;
USERNAMELEN : U S E R N A M E L E N;
USERPRIORITY : U S E R P R I O R I T Y;
USING : U S I N G;
VALIDATION : V A L I D A T I O N;
VALUE : V A L U E;
VALUELENGTH : V A L U E L E N G T H;
VERIFY : V E R I F Y;
VERSIONLEN : V E R S I O N L E N;
VOLUME : V O L U M E;
VOLUMELENG : V O L U M E L E N G;
WAIT : W A I T;
WAITCICS : W A I T C I C S;
WEB : W E B;
WPMEDIA : W P M E D I A;
WPMEDIA1 : W P M E D I A '1';
WPMEDIA2 : W P M E D I A '2';
WPMEDIA3 : W P M E D I A '3';
WPMEDIA4 : W P M E D I A '4';
WRAP : W R A P;
WRITE : W R I T E;
WRITEQ : W R I T E Q;
WSACONTEXT : W S A C O N T E X T;
WSAEPR : W S A E P R;
XCTL : X C T L;
XMLCONTAINER : X M L C O N T A I N E R;
XMLTODATA : X M L T O D A T A;
XMLTRANSFORM : X M L T R A N S F O R M;
XRBA : X R B A;
YEAR : Y E A R;
YYDDD : Y Y D D D;
YYDDMM : Y Y D D M M;
YYMMDD : Y Y M M D D;
YYYYDDD : Y Y Y Y D D D;
YYYYDDMM : Y Y Y Y D D M M;
YYYYMMDD : Y Y Y Y M M D D;
ZERO_DIGITAL : Z E R O '_' D I G I T A L;

// symbols
AMPCHAR : '&';
ASTERISKCHAR : '*';
DOUBLEASTERISKCHAR : '**';
COLONCHAR : ':';
COMMA_EOF : ',' EOF->skip;
COMMA_LB : ',' ('\r' | '\n' | '\f' | '\t' | ' ')+->skip;
COMMACHAR : ',';
COMMENTTAG : '*>';
COMMENTENTRYTAG : '*>CE';
DOLLARCHAR : '$';
DOUBLEQUOTE : '"';
// period full stopPosition
DOT_FS : '.' ('\r' | '\n' | '\f' | '\t' | ' ')+ | '.' EOF;
DOT : '.';
EQUALCHAR : '=';
LESSTHANCHAR : '<';
LESSTHANOREQUAL : '<=';
LPARENCHAR : '(';
MINUSCHAR : '-';
MORETHANCHAR : '>';
MORETHANOREQUAL : '>=';
NOTEQUALCHAR : '<>';
PLUSCHAR : '+';
SEMICOLON_FS : ';' ('\r' | '\n' | '\f' | '\t' | ' ')+ | ';' EOF;
SINGLEQUOTE : '\'';
RPARENCHAR : ')';
SLASHCHAR : '/';

INTEGERLITERAL : (PLUSCHAR | MINUSCHAR)? DIGIT+;

NUMERICLITERAL : (PLUSCHAR | MINUSCHAR)? DIGIT* (DOT | COMMACHAR) DIGIT+ (('e' | 'E') (PLUSCHAR | MINUSCHAR)? DIGIT+)?;

NONNUMERICLITERAL : UNTRMSTRINGLITERAL | STRINGLITERAL | DBCSLITERAL | HEXNUMBER | NULLTERMINATED;

//TXTLITERAL : STRINGLITERAL | IDENTIFIER;
CHAR_STRING_CONSTANT : HEXNUMBER | STRINGLITERAL;

IDENTIFIER : ([a-zA-Z0-9]+ ([-_]+ [a-zA-Z0-9]+)*);
FILENAME : IDENTIFIER+ '.' IDENTIFIER+; // TODO check , if this is OK?

// whitespace, line breaks, comments, ...
NEWLINE : '\r'? '\n' -> channel(HIDDEN);
COMMENTLINE : COMMENTTAG WS ~('\n' | '\r')* -> channel(HIDDEN);
COMMENTENTRYLINE : COMMENTENTRYTAG WS ~('\n' | '\r')*  -> channel(HIDDEN);
WS : [ \t\f;]+ -> channel(HIDDEN);
SEPARATOR : ', ' {!sqlFlag}?  -> channel(HIDDEN);

// treat all the non-processed tokens as errors
ERRORCHAR : . ;

ZERO_DIGIT: '0';


fragment HEXNUMBER :
	X '"' [0-9A-F]+ '"'
	| X '\'' [0-9A-F]+ '\''
;

fragment NULLTERMINATED :
	Z '"' (~["\n\r] | '""' | '\'')* '"'
	| Z '\'' (~['\n\r] | '\'\'' | '"')* '\''
;

fragment STRINGLITERAL :
	'"' (~["\n\r] | '""' | '\'')* '"'
	| '\'' (~['\n\r] | '\'\'' | '"')* '\''
;

fragment UNTRMSTRINGLITERAL :
	'"' (~["\n\r] | '""' | '\'')*
	| '\'' (~['\n\r] | '\'\'' | '"')*
;

fragment DBCSLITERAL :
	[GN] '"' (~["\n\r] | '""' | '\'')* '"'
	| [GN] '\'' (~['\n\r] | '\'\'' | '"')* '\''
;

fragment DIGIT: [0-9];
// case insensitive chars
fragment A:('a'|'A');
fragment B:('b'|'B');
fragment C:('c'|'C');
fragment D:('d'|'D');
fragment E:('e'|'E');
fragment F:('f'|'F');
fragment G:('g'|'G');
fragment H:('h'|'H');
fragment I:('i'|'I');
fragment J:('j'|'J');
fragment K:('k'|'K');
fragment L:('l'|'L');
fragment M:('m'|'M');
fragment N:('n'|'N');
fragment O:('o'|'O');
fragment P:('p'|'P');
fragment Q:('q'|'Q');
fragment R:('r'|'R');
fragment S:('s'|'S');
fragment T:('t'|'T');
fragment U:('u'|'U');
fragment V:('v'|'V');
fragment W:('w'|'W');
fragment X:('x'|'X');
fragment Y:('y'|'Y');
fragment Z:('z'|'Z');
