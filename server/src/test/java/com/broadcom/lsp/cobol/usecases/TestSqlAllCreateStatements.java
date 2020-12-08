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
 *    Broadcom, Inc. - initial API and implementation
 *
 */
package com.broadcom.lsp.cobol.usecases;

import com.broadcom.lsp.cobol.usecases.engine.UseCaseEngine;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

/** This PARAMETERIZED test checks if all below sql CREATE statements works correctly.
 *
 * <pre>
 * - CREATE ALIAS
 * - CREATE AUXILIARY TABLE
 * - CREATE DATABASE
 * - CREATE FUNCTION
 * - CREATE FUNCTION (compiled SQL scalar)
 * - CREATE FUNCTION (external scalar)
 * - CREATE FUNCTION (external table)
 * - CREATE FUNCTION (inlined SQL scalar)
 * - CREATE FUNCTION (sourced)
 * - CREATE FUNCTION (SQL table)
 * - CREATE GLOBAL TEMPORARY TABLE
 * - CREATE INDEX
 * - CREATE LOB TABLESPACE
 * - CREATE MASK
 * - CREATE PERMISSION
 * - CREATE PROCEDURE (external)
 * - CREATE PROCEDURE (sql native)
 * - CREATE ROLE
 * - CREATE SEQUENCE
 * - CREATE STOGROUP
 * - CREATE TABLE
 * - CREATE TABLESPACE
 * - CREATE TRIGGER (advanced)
 * - CREATE TRIGGER (basic)
 * - CREATE CREATE TRUSTED CONTEXT
 * - CREATE TRUSTED CONTEXT
 * - CREATE CREATE TYPE (array)
 * - CREATE CREATE TYPE (distinct)
 * - CREATE VARIABLE
 * - CREATE VIEW
 * </pre>
 * */
class TestSqlAllCreateStatements {

  // CREATE: ALIAS
  private static final String CREATE_ALIAS =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // CREATE ALIAS LATABLES FOR DB2USCALABOA5281.SYSIBM.SYSTABLES;
      ;

  private static final String CREATE_ALIAS2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // CREATE ALIAS LATABLES FOR DB2USCALABOA5281.SYSIBM.SYSTABLES;
      ;

  // CREATE AUXILIARY TABLE
  private static final String CREATE_AUX_TABLE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // CREATE AUX TABLE EMP_PHOTO_ATAB
      //      IN DSN8D12A.PHOTOLTS
      //      STORES DSN8C10.EMP
      //      COLUMN EMP_PHOTO;
      ;

  // CREATE: DATABASE
  private static final String CREATE_DB =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // CREATE DATABASE DSN8D12P
      //     STOGROUP DSN8G120
      //     BUFFERPOOL BP8K1
      //     INDEXBP BP2;
      ;

  private static final String CREATE_DB2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // CREATE DATABASE DSN8TEMP
      //     CCSID ASCII;
      ;

  // CREATE FUNCTION (compiled SQL scalar)
  private static final String CREATE_FUNCTION_COMPILED =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // CREATE FUNCTION REVERSE(INSTR VARCHAR(4000))
      //      RETURNS VARCHAR(4000)
      //      DETERMINISTIC NO EXTERNAL ACTION CONTAINS SQL
      //      BEGIN
      //      DECLARE REVSTR, RESTSTR VARCHAR(4000) DEFAULT '';
      //      DECLARE LEN INT;
      //      IF INSTR IS NULL THEN
      //      RETURN NULL;
      //      END IF;
      //      SET (RESTSTR, LEN) = (INSTR, LENGTH(INSTR));
      //      WHILE LEN > 0 DO
      //      SET (REVSTR, RESTSTR, LEN)
      //        = (SUBSTR(RESTSTR, 1, 1) CONCAT REVSTR,
      //        SUBSTR(RESTSTR, 2, LEN - 1),
      //        LEN - 1);
      //     END WHILE;
      //     RETURN REVSTR;
      //   END#
      ;

  //CREATE FUNCTION external scalar
  private static final String CREATE_FUNCTION_EXT =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // CREATE FUNCTION NTEST1 (SMALLINT)
      //      RETURNS SMALLINT
      //      EXTERNAL NAME 'NTESTMOD'
      //      SPECIFIC MINENULL1
      //      LANGUAGE C
      //      DETERMINISTIC
      //      NO SQL
      //      FENCED
      //      PARAMETER STYLE SQL
      //      RETURNS NULL ON NULL INPUT
      //      NO EXTERNAL ACTION;
      ;

  private static final String CREATE_FUNCTION_EXT2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  CREATE FUNCTION CENTER (INTEGER, FLOAT)
      //      RETURNS FLOAT
      //      EXTERNAL NAME 'MIDDLE'
      //      LANGUAGE C
      //      DETERMINISTIC
      //      NO SQL
      //      FENCED
      //      PARAMETER STYLE SQL
      //      NO EXTERNAL ACTION
      //      STAY RESIDENT YES;
      ;

  private static final String CREATE_FUNCTION_EXT3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // CREATE FUNCTION SMITH.CENTER (FLOAT, FLOAT, FLOAT)
      //      RETURNS DECIMAL(8,4) CAST FROM FLOAT
      //      EXTERNAL NAME 'CMOD'
      //      SPECIFIC FOCUS98
      //      LANGUAGE C
      //      DETERMINISTIC
      //      NO SQL
      //      FENCED
      //      PARAMETER STYLE SQL
      //      NO EXTERNAL ACTION
      //      SCRATCHPAD
      //      NO FINAL CALL;
      ;

  private static final String CREATE_FUNCTION_EXT4 =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          // CREATE FUNCTION FINDV (CLOB(100K))
          //      RETURNS INTEGER
          //      FENCED
          //      LANGUAGE JAVA
          //      PARAMETER STYLE JAVA
          //      EXTERNAL NAME 'JAVAUDFS.FINDVWL'
          //      NO EXTERNAL ACTION
          //      CALLED ON NULL INPUT
          //      DETERMINISTIC
          //      NO SQL;
          ;

  // CREATE FUNCTION external table
  private static final String CREATE_FUNCTION_EXT_TABLE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  CREATE FUNCTION DOCMATCH (VARCHAR(30), VARCHAR(255))
      //                       RETURNS TABLE (DOC_ID CHAR(16))
      //      EXTERNAL NAME ABC
      //      LANGUAGE C
      //      PARAMETER STYLE SQL
      //      NO SQL
      //      DETERMINISTIC
      //      NO EXTERNAL ACTION
      //      FENCED
      //      SCRATCHPAD
      //      FINAL CALL
      //      DISALLOW PARALLEL
      //      CARDINALITY 20;
      ;

  private static final String CREATE_FUNCTION_EXT_TABLE2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // CREATE FUNCTION tf6(p1 VARCHAR(10))
      //	RETURNS GENERIC TABLE
      //	EXTERNAL NAME 'tf6'
      //	LANGUAGE C
      //	PARAMETER STYLE SQL
      //	DETERMINISTIC
      //	NO EXTERNAL ACTION
      //	FENCED
      //	SCRATCHPAD
      //	FINAL CALL;
      ;

  // CREATE FUNCTION (inlined SQL scalar)
  private static final String CREATE_FUNCTION_INLINED =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  CREATE FUNCTION TAN (X DOUBLE)
      //      RETURNS DOUBLE
      //      LANGUAGE SQL
      //      CONTAINS SQL
      //      NO EXTERNAL ACTION
      //      DETERMINISTIC
      //      RETURN SIN(X)/COS(X);
      ;

  // CREATE FUNCTION SOURCED
  private static final String CREATE_FUNCTION_SOURCED =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  CREATE FUNCTION AVE (HATSIZE) RETURNS HATSIZE
      //      SOURCE SYSIBM.AVG (INTEGER);
      ;

  private static final String CREATE_FUNCTION_SOURCED2 =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          // CREATE FUNCTION MYCENTER (INTEGER, INTEGER)
  //      RETURNS FLOAT
  //      SOURCE SMITH.CENTER (INTEGER, FLOAT);
  ;


  // CREATE FUNCTION (SQL table)
  private static final String CREATE_FUNCTION_SQL_TABLE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // CREATE FUNCTION JTABLE (COLD_VALUE CHAR(9), T2_FLAG CHAR(1))
          //	RETURNS TABLE (COLA INT, COLB INT, COLC INT)
          //	LANGUAGE SQL
          //	SPECIFIC DEPTINFO
          //	NOT DETERMINISTIC
          //	READS SQL DATA
          //	RETURN
          //		SELECT A.COLA, B.COLB, B.COLC
          //			FROM TABLE1 AS A
          //				LEFT OUTER JOIN
          //				TABLE2 AS B
          //			ON A.COL1 = B.COL1 AND T2_FLAG = 'Y'
          //		WHERE A.COLD = COLD_VALUE;
      ;

  // CREATE FUNCTION (SQL table)
  private static final String CREATE_FUNCTION_SQL_TABLE2 =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          // CREATE FUNCTION DEPTEMPLOYEES (DEPTNO CHAR(3))
          //	RETURNS TABLE (EMPNO CHAR(6), LASTNAME VARCHAR(15), FIRSTNAME VARCHAR(12))
          //	LANGUAGE SQL
          //	READS SQL DATA
          //	NO EXTERNAL ACTION
          //	DETERMINISTIC
          //	RETURN
          //		SELECT EMPNO, LASTNAME, FIRSTNME
          //			FROM YEMP
          //		WHERE YEMP.WORKDEPT = DEPTEMPLOYEES.DEPTNO;
          ;

  //CREATE GLOBAL TEMPORARY TABLE
  private static final String CREATE_GLOBAL_TMP_TABLE =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          // CREATE GLOBAL TEMPORARY TABLE CURRENTMAP
          //     (CODE INTEGER NOT NULL, MEANING VARCHAR(254) NOT NULL);
          ;

  private static final String CREATE_GLOBAL_TMP_TABLE2 =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          // CREATE GLOBAL TEMPORARY TABLE EMP
          //     (TMPDEPTNO   CHAR(3)     NOT NULL,
          //      TMPDEPTNAME VARCHAR(36) NOT NULL,
          //      TMPMGRNO    CHAR(6)             ,
          //      TMPLOCATION CHAR(16)
          ;

  // CREATE INDEX
  private static final String CREATE_INDEX =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // CREATE UNIQUE INDEX DSN8C10.XDEPT1
          //     ON DSN8C10.DEPT
          //       (DEPTNO ASC)
          //     PADDED
          //     USING STOGROUP DSN8G120
          //       PRIQTY 512
          //       SECQTY 64
          //       ERASE NO
          //     BUFFERPOOL BP1
          //     CLOSE YES
          //     PIECESIZE 1M;
      ;

  private static final String CREATE_INDEX2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // CREATE INDEX DSN8C10.XEMP2
          //     ON DSN8C10.EMP
          //       (EMPNO ASC)
          //     USING STOGROUP DSN8G120
          //       PRIQTY 36
          //       ERASE NO
          //       CLUSTER
          //       PARTITION BY RANGE
          //       (PARTITION 1 ENDING AT('H99'),
          //        PARTITION 2 ENDING AT('P99'),
          //        PARTITION 3 ENDING AT('Z99'),
          //        PARTITION 4 ENDING AT('999'))
          //     BUFFERPOOL BP1
          //     CLOSE YES
          //     COPY YES;
      ;

  private static final String CREATE_INDEX3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // CREATE UNIQUE INDEX DSN8C10.XDEPT1
          //    ON DSN8C10.DEPT
          //      (DEPTNO ASC)
          //    USING VCAT DSNCAT
          //    PIECESIZE 1048576K;
      ;

  private static final String CREATE_INDEX4 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // CREATE UNIQUE INDEX DSN8C10.XPHOTO
          //     ON DSN8C10.EMP_PHOTO_ATAB
          //     USING VCAT DSNCAT
          //     COPY YES;
      ;


  //CREATE LOB TABLESPACE
  private static final String CREATE_LOB_TABLESPACE =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          // CREATE LOB TABLESPACE PHOTOLTS
          //     IN DSN8D12A
          //     USING STOGROUP DSN8G120
          //       PRIQTY 3200
          //       SECQTY 1600
          //     LOCKSIZE LOB
          //     BUFFERPOOL BP16K0
          //     GBPCACHE SYSTEM
          //     NOT LOGGED
          //     CLOSE NO;
          ;


  // CREATE MASK
  private static final String CREATE_MASK =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          // CREATE MASK SSN_MASK ON EMPLOYEE
          //   FOR COLUMN SSN RETURN
          //     CASE
          //          WHEN (VERIFY_GROUP_FOR_USER(SESSION_USER,'PAYROLL') = 1)
          //			      THEN SSN
          //          WHEN (VERIFY_GROUP_FOR_USER(SESSION_USER,'MGR') = 1)
          //           THEN 'XXX-XX-' || SUBSTR(SSN,8,4)
          //          ELSE NULL
          //     END
          //   ENABLE;
          //
          //COMMIT;
          //
          //CREATE TABLE EMPLOYEE
          //    ACTIVATE COLUMN ACCESS CONTROL;
          //
          //COMMIT;
          //
          //SELECT SSN FROM EMPLOYEE
          //    WHERE EMPNO = 123456;
          ;

  private static final String CREATE_MASK2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // CREATE MASK SSN_MASK ON EMPLOYEE
          //    FOR COLUMN SSN RETURN
          //      CASE
          //           WHEN (1 = 1)
          //            THEN 'XXX-XX-' || SUBSTR(SSN,8,4)
          //           ELSE NULL
          //      END
          //    ENABLE;
          //
          //COMMIT;
          //
          //CREATE TABLE EMPLOYEE
          //    ACTIVATE COLUMN ACCESS CONTROL;
          //
          //COMMIT;
          //
          //SELECT 'XXX-XX-' || SUBSTR(SSN,8,4) FROM EMPLOYEE
          //    WHERE EMPNO = 123456;
      ;

  private static final String CREATE_MASK3 =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          // CREATE MASK CITY_MASK ON LIBRARY_USAGE
          //    FOR COLUMN CITY RETURN
          //      CASE
          //           WHEN (LIBRARY_OPT = 'OPT-IN')
          //            THEN CITY
          //           ELSE ' '
          //      END
          //    ENABLE;
          //
          //COMMIT;
          //
          //CREATE TABLE LIBRARY_USAGE
          //   ACTIVATE COLUMN ACCESS CONTROL;
          //
          //COMMIT;
          //
          //SELECT CITY, AVG(LIBRARY_TIME) FROM LIBRARY_USAGE
          //   GROUP BY CITY;
          ;

  private static final String CREATE_MASK4 =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          //CREATE MASK SALARY_MASK ON EMPLOYEE
          //   FOR COLUMN SALARY RETURN
          //       CASE
          //            WHEN (BONUS < 10000)
          //             THEN SALARY
          //            ELSE NULL
          //       END
          //   ENABLE;
          //
          //COMMIT;
          //
          //CREATE MASK BONUS_MASK ON EMPLOYEE
          //   FOR COLUMN BONUS RETURN
          //       CASE
          //            WHEN (BONUS > 5000)
          //             THEN NULL
          //            ELSE BONUS
          //       END
          //   ENABLE;
          //
          //COMMIT;
          //
          //CREATE TABLE EMPLOYEE
          //    ACTIVATE COLUMN ACCESS CONTROL;
          //
          //COMMIT;
          //
          //SELECT SALARY FROM EMPLOYEE
          //    WHERE EMPNO = 123456;
          ;

  private static final String CREATE_MASK5 =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          //CREATE EMPLOYEE (EMPID INT,
          //                 DEPTID CHAR(8),
          //                 SALARY DEC(9,2) NOT NULL,
          //                 BONUS DEC(9,2));
          //
          //CREATE MASK SALARY_MASK ON EMPLOYEE
          //    FOR COLUMN SALARY RETURN
          //       CASE
          //            WHEN SALARY < 10000
          //             THEN CAST(SALARY*2 AS DEC(9,2))
          //            ELSE COALESCE(CAST(SALARY/2 AS DEC(9,2)), BONUS)
          //       END
          //    ENABLE;
          //
          //COMMIT;
          //
          //CREATE MASK BONUS_MASK ON EMPLOYEE
          //    FOR COLUMN BONUS RETURN
          //      CASE
          //          WHEN BONUS > 1000
          //           THEN BONUS
          //          ELSE NULL
          //      END
          //    ENABLE;
          //
          //COMMIT;
          //
          //CREATE TABLE EMPLOYEE
          //    ACTIVATE COLUMN ACCESS CONTROL;
          //
          //COMMIT;
          //
          //SELECT SALARY FROM DEPT
          //    LEFT JOIN EMPLOYEE ON DEPTNO = DEPTID;
          //
          ///* When SALARY_MASK is merged into the above statement,
          // * 'WHEN SALARY IS NULL THEN NULL' is added as the
          // * first WHEN clause, as follows:
          // */
          //
          //SELECT CASE WHEN SALARY IS NULL THEN NULL
          //            WHEN SALARY < 10000 THEN CAST(SALARY*2 AS DEC(9,2))
          //            ELSE COALESCE(CAST(SALARY/2 AS DEC(9,2)), BONUS)
          //       END SALARY
          //       FROM DEPT
          //         LEFT JOIN EMPLOYEE ON DEPTNO = DEPTID;
          ;

  // CREATE PERMISSION
  private static final String CREATE_PERMISSION =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //CREATE PERMISSION SALARY_ROW_ACCESS ON EMPLOYEE
          //   FOR ROWS WHERE VERIFY_GROUP_FOR_USER(SESSION_USER,'MGR','ACCOUNTING') = 1
          //            AND
          //            ACCOUNTING_UDF(SALARY) < 120000
          //   ENFORCED FOR ALL ACCESS
          //   ENABLE;
          //
          //COMMIT;
          //
          //CREATE TABLE EMPLOYEE
          //	ACTIVATE ROW ACCESS CONTROL;
          //
          //COMMIT;
          //
          //SELECT SALARY FROM EMPLOYEE
          //   WHERE EMPNO = 123456;
      ;

  private static final String CREATE_PERMISSION2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // CREATE PERMISSION TELLER_ROW_ACCESS ON CUSTOMER
          //   FOR ROWS WHERE VERIFY_GROUP_FOR_USER(SESSION_USER,'TELLER') = 1
          //            AND
          //            BRANCH = (SELECT HOME_BRANCH FROM INTERNAL_INFO
          //                         WHERE EMP_ID = SESSION_USER)
          //    ENFORCED FOR ALL ACCESS
          //    ENABLE;
          //
          //COMMIT;
          //
          //CREATE PERMISSION CSR_ROW_ACCESS ON CUSTOMER
          //   FOR ROWS WHERE VERIFY_GROUP_FOR_USER(SESSION_USER,'CSR') = 1
          //   ENFORCED FOR ALL ACCESS
          //   ENABLE;
          //
          //COMMIT;
          //
          //CREATE TABLE CUSTOMER
          //   ACTIVATE ROW ACCESS CONTROL;
          //
          //COMMIT;
          //
          //SELECT * FROM CUSTOMER;
      ;

  // CREATE PROCEDURE external
  private static final String CREATE_PROCEDURE_EXT =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // CREATE PROCEDURE SYSPROC.MYPROC(IN INT, OUT INT, OUT DECIMAL(7,2))
          //         LANGUAGE COBOL
          //         EXTERNAL NAME MYMODULE
          //         PARAMETER STYLE GENERAL
          //         WLM ENVIRONMENT PARTSA
          //         DYNAMIC RESULT SETS 1;
      ;

  private static final String CREATE_PROCEDURE_EXT2 =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          // CREATE PROCEDURE SYSPROC.MYPROC(IN INT, OUT INT, OUT DECIMAL(7,2))
          //         LANGUAGE COBOL
          //         EXTERNAL NAME MYMODULE
          //         PARAMETER STYLE SQL
          //         WLM ENVIRONMENT PARTSA
          //         DYNAMIC RESULT SETS 1
          //         RUN OPTIONS 'HEAP(,,ANY),BELOW(4K,,),ALL31(ON),STACK(,,ANY,)';
          ;

  private static final String CREATE_PROCEDURE_EXT3 =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          // CREATE PROCEDURE PARTS_ON_HAND(IN PARTNUM INT,
          //                                 OUT COST DECIMAL(7,2),
          //                                 OUT QUANTITY INT)
          //         LANGUAGE JAVA
          //         EXTERNAL NAME 'PARTS.ONHAND'
          //         PARAMETER STYLE JAVA;
          ;


  // CREATE PROCEDURE SQL external
  private static final String CREATE_PROCEDURE_SQL_NATIVE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // CREATE PROCEDURE UPDATE_SALARY_1
          // (IN EMPLOYEE_NUMBER CHAR(10),
          // IN RATE DECIMAL(6,2))
          // LANGUAGE SQL
          // MODIFIES SQL DATA
          //  UPDATE EMP
          //  SET SALARY = SALARY * RATE
          //  WHERE EMPNO = EMPLOYEE_NUMBER
      ;

  private static final String CREATE_PROCEDURE_SQL_NATIVE2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // CREATE PROCEDURE UPDATE_SALARY_1
          // (IN EMPLOYEE_NUMBER CHAR(10),
          // IN RATE DECIMAL(6,2))
          // LANGUAGE SQL
          // MODIFIES SQL DATA
          // DETERMINISTIC
          // COMMIT ON RETURN YES
          //   UPDATE EMP
          //   SET SALARY = SALARY * RATE
          //   WHERE EMPNO = EMPLOYEE_NUMBER
      ;


  // CREATE ROLE
  private static final String CREATE_ROLE =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          // CREATE ROLE TELLER;
          ;


  // CREATE SEQUENCE
  private static final String CREATE_SEQUENCE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  CREATE SEQUENCE ORDER_SEQ
          //      START WITH 1
          //      INCREMENT BY 1
          //      NO MAXVALUE
          //      NO CYCLE
          //      CACHE 24;
      ;

  private static final String CREATE_SEQUENCE2 =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          // CREATE SEQUENCE ORDER_SEQ
          //      START WITH 1
          //      INCREMENT BY 1
          //      NO MAXVALUE
          //      NO CYCLE
          //      CACHE 20;
          //     INSERT INTO ORDERS (ORDERNO, CUSTNO)
          //       VALUES (NEXT VALUE FOR ORDER_SEQ, 123456);
          ;

  // CREATE STOGROUP
  private static final String CREATE_STOGROUP =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  CREATE STOGROUP DSN8G120
          //     VOLUMES (ABC005,DEF008)
          //     VCAT DSNCAT;
      ;

  private static final String CREATE_STOGROUP2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // CREATE STOGROUP DSNCG100
          //     VOLUMES (ABC001,DEF003) VCAT DSNCAT
          //     KEY LABEL STG01KLABEL;
      ;

  // CREATE TABLE
  private static final String CREATE_TABLE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // CREATE TABLE DSN8C10.DEPT
          //     (DEPTNO   CHAR(3)     NOT NULL,
          //      DEPTNAME VARCHAR(36) NOT NULL,
          //      MGRNO    CHAR(6)             ,
          //      ADMRDEPT CHAR(3)     NOT NULL,
          //      LOCATION CHAR(16)            ,
          //      PRIMARY KEY(DEPTNO)          )
          //     IN DSN8D12A.DSN8S12D;
      ;

  private static final String CREATE_TABLE2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // CREATE TABLE DSN8C10.PROJ
          //     (PROJNO   CHAR(6)      NOT NULL,
          //      PROJNAME VARCHAR(24)  NOT NULL,
          //      DEPTNO   CHAR(3)      NOT NULL,
          //      RESPEMP  CHAR(6)      NOT NULL,
          //      PRSTAFF  DECIMAL(5,2)         ,
          //      PRSTDATE DATE                 ,
          //      PRENDATE DATE                 ,
          //      MAJPROJ  CHAR(6)      NOT NULL)
          //     IN DATABASE DSN8D12A
          //     VALIDPROC DSN8EAPR;
      ;

  private static final String CREATE_TABLE3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  CREATE TABLE ACTIVITY
          //     (PROJNO   CHAR(6)      NOT NULL,
          //      ACTNO    SMALLINT     NOT NULL,
          //      ACTDEPT  CHAR(3)      NOT NULL,
          //      ACTOWNER CHAR(6)      NOT NULL,
          //      ACSTAFF  DECIMAL(5,2)         ,
          //      ACSTDATE DATE         NOT NULL,
          //      ACENDATE DATE                 ,
          //      FOREIGN KEY (ACTDEPT,ACTOWNER)
          //         REFERENCES PROJECT (DEPTNO,RESPEMP) ON DELETE RESTRICT)
          //     IN DSN8D12A.DSN8S12D;
      ;

  private static final String CREATE_TABLE4 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // CREATE TABLE DSN8C10.EMP_PHOTO_RESUME
          //     (EMPNO      CHAR(6)     NOT NULL,
          //      EMP_ROWID  ROWID NOT NULL GENERATED ALWAYS,
          //      EMP_PHOTO  BLOB(110K),
          //      RESUME     CLOB(5K),
          //      PRIMARY KEY (EMPNO))
          //     IN DSN8D12A.DSN8S12E
          //     CCSID EBCDIC;
      ;

  private static final String CREATE_TABLE5 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // CREATE TABLE EMPLOYEE
          //     (EMPNO      INTEGER GENERATED ALWAYS AS IDENTITY,
          //      ID         SMALLINT,
          //      NAME       CHAR(30),
          //      SALARY     DECIMAL(5,2),
          //      DEPTNO     SMALLINT)
          //     IN DSN8D12A.DSN8S12D;
      ;

  private static final String CREATE_TABLE6 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // CREATE TABLE STRANS AS
          //     (SELECT YEAR AS SYEAR, MONTH AS SMONTH, DAY AS SDAY, SUM(AMOUNT) AS SSUM
          //      FROM TRANS
          //      GROUP BY YEAR, MONTH, DAY)
          //      DATA INITIALLY DEFERRED REFRESH DEFERRED;
      ;

  private static final String CREATE_TABLE7 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  CREATE TABLE TS01TB
          //       (C1 SMALLINT,
          //        C2 DECIMAL(9,2),
          //        C3 CHAR(4))
          //    APPEND YES
          //    IN TS01DB.TS01TS;
      ;

  private static final String CREATE_TABLE8 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  CREATE TABLE TS02TB
          //       (C1 SMALLINT,
          //        C2 DECIMAL(9,2),
          //        C3 CHAR(4))
          //     PARTITION BY SIZE EVERY 4G
          //     IN DATABASE DSNDB04;
      ;

  private static final String CREATE_TABLE9 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // CREATE TABLE EMP_INFO
          //       (EMPNO CHAR(6) NOT NULL,
          //        EMP_INFOCHANGE NOT NULL
          //           GENERATED ALWAYS FOR EACH ROW ON UPDATE
          //           AS ROW CHANGE TIMESTAMP,
          //        EMP_ADDRESS VARCHAR(300),
          //        EMP_PHONENO CHAR(4),
          //        PRIMARY KEY (EMPNO));
      ;

  private static final String CREATE_TABLE10 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //CREATE TABLE TB01 (
          //   ACCT_NUM         INTEGER,
          //   CUST_LAST_NM     CHAR(15),
          //   LAST_ACTIVITY_DT VARCHAR(25),
          //   COL2             CHAR(10),
          //   COL3             CHAR(25),
          //   COL4             CHAR(25),
          //   COL5             CHAR(25),
          //   COL6             CHAR(55),
          //   STATE            CHAR(55))
          // IN DBB.TS01
          //
          //  PARTITION BY (ACCT_NUM)
          //   (PARTITION 1 ENDING AT (199),
          //    PARTITION 2 ENDING AT (299),
          //    PARTITION 3 ENDING AT (399),
          //    PARTITION 4 ENDING AT (MAXVALUE));
      ;


  // CREATE TABLESPACE
  private static final String CREATE_TABLESPACE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // CREATE TABLESPACE DSN8S12D
          //     IN DSN8D12A
          //     USING STOGROUP DSN8G120
          //       PRIQTY 52
          //       SECQTY 20
          //       ERASE NO
          //     LOCKSIZE PAGE
          //     BUFFERPOOL BP1
          //     CLOSE YES;
      ;

  private static final String CREATE_TABLESPACE2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // CREATE TABLESPACE SALESHX
          //     IN DSN8D12A
          //     USING STOGROUP DSN8G120
          //       PRIQTY 4000
          //       SECQTY 130
          //       ERASE NO
          //     NUMPARTS 82
          //     (PARTITION 80
          //       COMPRESS YES,
          //      PARTITION 81
          //       COMPRESS YES,
          //      PARTITION 82
          //       COMPRESS YES
          //       ERASE YES)
          //     LOCKSIZE PAGE
          //     BUFFERPOOL BP1
          //     CLOSE NO;
      ;

  private static final String CREATE_TABLESPACE3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  CREATE TABLESPACE TS1
          //     IN DSN8D12A
          //     USING STOGROUP DSN8G120
          //     NUMPARTS 55
          //     SEGSIZE 16
          //     LOCKSIZE ANY;
      ;

  private static final String CREATE_TABLESPACE4 =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          // CREATE TABLESPACE TS2
          //     IN DSN8D12A
          //     USING STOGROUP DSN8G120
          //     NUMPARTS 7
          //     (
          //      PARTITION 1 COMPRESS YES,
          //      PARTITION 3 COMPRESS YES,
          //      PARTITION 5 COMPRESS YES,
          //      PARTITION 7 COMPRESS YES
          //     )
          //     SEGSIZE 64
          //     DEFINE NO;
          ;

  // CREATE TRIGGER ADVANCED
  private static final String CREATE_TRIGGER_ADV =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // CREATE TRIGGER NEW_HIRE
          //      AFTER INSERT ON EMPLOYEE
          //      FOR EACH ROW
          //      BEGIN ATOMIC
          //        UPDATE COMPANY_STATS SET NBEMP = NBEMP + 1;
          //      END
      ;

  private static final String CREATE_TRIGGER_ADV2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //CREATE TRIGGER REORDER
          //     AFTER UPDATE OF ON_HAND, MAX_STOCKED ON PARTS
          //     REFERENCING NEW AS NROW
          //     FOR EACH ROW
          //     WHEN (NROW.ON_HAND < 0.10 * NROW.MAX_STOCKED)
          //     BEGIN ATOMIC
          //       DECLARE QTY_ORDERED INTEGER;
          //
          //       VALUES(ISSUE_SHIP_REQUEST(NROW.MAX_STOCKED - NROW.ON_HAND, NROW.PARTNO))
          //         INTO QTY_ORDERED;
          //     END
      ;

  private static final String CREATE_TRIGGER_ADV3 =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          //CREATE TRIGGER REORDER
          //     AFTER UPDATE OF ON_HAND, MAX_STOCKED ON PARTS
          //     REFERENCING NEW_TABLE AS NTABLE
          //     FOR EACH STATEMENT
          //       BEGIN ATOMIC
          //         DECLARE QTY_ORDERED INTEGER;
          //
          //         SELECT ISSUE_SHIP_REQUEST(MAX_STOCKED - ON_HAND, PARTNO)
          //           FROM NTABLE
          //         WHERE (ON_HAND < 0.10 * MAX_STOCKED)
          //           INTO QTY_ORDERED;
          //     END
          ;

  private static final String CREATE_TRIGGER_ADV4 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // CREATE TRIGGER SAL_ADJ
          //     AFTER UPDATE OF SALARY ON EMPLOYEE
          //     REFERENCING OLD AS OLD_EMP
          //                 NEW AS NEW_EMP
          //     FOR EACH ROW
          //     WHEN (NEW_EMP.SALARY > (OLD_EMP.SALARY * 1.20))
          //       BEGIN ATOMIC
          //         SIGNAL SQLSTATE '75001' ('Invalid Salary Increase - Exceeds 20
          //       END
      ;

  private static final String CREATE_TRIGGER_ADV5 =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          // CREATE TABLE WEATHER
          //      (CITY VARCHAR(25),
          //       TEMPF DECIMAL(5,2));
          //   CREATE VIEW CELSIUS_WEATHER (CITY, TEMPC) AS
          //      SELECT CITY, (TEMPF-32)/1.8
          //      FROM WEATHER;
          ;


  // CREATE TRIGGER BASIC
  private static final String CREATE_TRIGGER =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // CREATE TRIGGER NEW_HIRE
          //      AFTER INSERT ON EMPLOYEE
          //      FOR EACH ROW MODE DB2SQL
          //      BEGIN ATOMIC
          //        UPDATE COMPANY_STATS SET NBEMP = NBEMP + 1;
          //      END
      ;

  private static final String CREATE_TRIGGER2 =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          // CREATE TRIGGER REORDER
          //     AFTER UPDATE OF ON_HAND, MAX_STOCKED ON PARTS
          //     REFERENCING NEW AS NROW
          //     FOR EACH ROW
          //     MODE DB2SQL
          //     WHEN (NROW.ON_HAND < 0.10 * NROW.MAX_STOCKED)
          //     BEGIN ATOMIC
          //       VALUES(ISSUE_SHIP_REQUEST(NROW.MAX_STOCKED - NROW.ON_HAND, NROW.PARTNO));
          //     END
          ;

  private static final String CREATE_TRIGGER3 =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          // CREATE TRIGGER REORDER
          //     AFTER UPDATE OF ON_HAND, MAX_STOCKED ON PARTS
          //     REFERENCING NEW_TABLE AS NTABLE
          //     FOR EACH STATEMENT MODE DB2SQL
          //       BEGIN ATOMIC
          //         SELECT ISSUE_SHIP_REQUEST(MAX_STOCKED - ON_HAND, PARTNO)
          //           FROM NTABLE
          //         WHERE (ON_HAND < 0.10 * MAX_STOCKED);
          //     END
          ;

  private static final String CREATE_TRIGGER4 =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          //CREATE TRIGGER SAL_ADJ
          //     AFTER UPDATE OF SALARY ON EMPLOYEE
          //     REFERENCING OLD AS OLD_EMP
          //                 NEW AS NEW_EMP
          //     FOR EACH ROW MODE DB2SQL
          //     WHEN (NEW_EMP.SALARY > (OLD_EMP.SALARY * 1.20))
          //       BEGIN ATOMIC
          //         SIGNAL SQLSTATE '75001' ('Invalid Salary Increase - Exceeds 20
          //       END
          ;

  private static final String CREATE_TRIGGER5 =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          // CREATE TABLE WEATHER
          //      (CITY VARCHAR(25),
          //       TEMPF DECIMAL(5,2));
          //   CREATE VIEW CELSIUS_WEATHER (CITY, TEMPC) AS
          //      SELECT CITY, (TEMPF-32)/1.8
          //      FROM WEATHER;
          ;

  // CREATE TRUSTED CONTEXT
  private static final String CREATE_TRUSTED_CONTEXT =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  CREATE TRUSTED CONTEXT CTX1
          //       BASED UPON CONNECTION USING SYSTEM AUTHID ADMF001
          //       ATTRIBUTES (ADDRESS '9.30.131.203',
          //                   ENCRYPTION 'LOW')
          //       DEFAULT ROLE CTXROLE
          //       ENABLE
          //       WITH USE FOR SAM, JOE ROLE ROLE1 WITH AUTHENTICATION;
      ;

  private static final String CREATE_TRUSTED_CONTEXT2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // CREATE TRUSTED CONTEXT CTX2
          //        BASED UPON CONNECTION USING SYSTEM AUTHID ADMF002
          //        ATTRIBUTES (JOBNAME 'WASPROD')
          //        DEFAULT ROLE CTXROLE WITH ROLE AS OBJECT OWNER AND QUALIFIER
          //        ENABLE
          //        WITH USE FOR SALLY;
      ;


  // CREATE TYPE array
  private static final String CREATE_TYPE_ARRAY =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          // CREATE TYPE PHONENUMBERS AS DECIMAL(10,0) ARRAY[50];
          ;

  private static final String CREATE_TYPE_ARRAY2 =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          // CREATE TYPE GENERIC.NUMBERS AS DECFLOAT(34) ARRAY[];
          ;

  private static final String CREATE_TYPE_ARRAY3 =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          // CREATE TYPE PERSONAL_PHONENUMBERS AS DECIMAL(16,0) ARRAY[VARCHAR(8)];
          ;

  private static final String CREATE_TYPE_ARRAY4 =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          // CREATE TYPE CAPITALSARRAY AS VARCHAR(30) ARRAY[VARCHAR(20)];
          ;

  private static final String CREATE_TYPE_ARRAY5 =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          // CREATE TYPE PRODUCTS AS VARCHAR(40) ARRAY[INTEGER];
          ;


  // CREATE TYPE distinct
  private static final String CREATE_TYPE_DISTINCT =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          //   CREATE TYPE SHOESIZE AS INTEGER;
          ;

  private static final String CREATE_TYPE_DISTINCT2 =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          //  CREATE TYPE MILES AS DOUBLE;
          ;


  // CREATE VARIABLE
  private static final String CREATE_VARIABLE  =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          //  CREATE VARIABLE MYSCHEMA.MYJOB_PRINTER VARCHAR(30)
          // DEFAULT 'Default printer';
          ;

  private static final String CREATE_VARIABLE2 =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          //   CREATE VARIABLE SCHEMA1.GV_DEPTNO INTEGER
          // DEFAULT 'Unassigned';
          ;


  // CREATE VIEW
  private static final String CREATE_VIEW =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  CREATE VIEW DSN8C10.VPROJRE1
          //     (PROJNO,PROJNAME,PROJDEP,RESPEMP,
          //      FIRSTNME,MIDINIT,LASTNAME)
          //     AS SELECT ALL
          //     PROJNO,PROJNAME,DEPTNO,EMPNO,
          //     FIRSTNME,MIDINIT,LASTNAME
          //     FROM DSN8C10.PROJ, DSN8C10.EMP
          //     WHERE RESPEMP = EMPNO;
      ;

  private static final String CREATE_VIEW2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  CREATE VIEW DSN8C10.FIRSTQTR (SNO, CHARGES, DATE) AS
          //  SELECT SNO, CHARGES, DATE
          //  FROM MONTH1
          //  WHERE DATE BETWEEN '01/01/2000' and '01/31/2000'
          //    UNION All
          //  SELECT SNO, CHARGES, DATE
          //  FROM MONTH2
          //  WHERE DATE BETWEEN '02/01/2000' and '02/29/2000'
          //    UNION All
          //  SELECT SNO, CHARGES, DATE
          //  FROM MONTH3
          //  WHERE DATE BETWEEN '03/01/2000' and '03/31/2000';
      ;

  private static Stream<String> textsToTest() {
    // add all
    return Stream.of(
        CREATE_DB, CREATE_FUNCTION_EXT, CREATE_FUNCTION_EXT2, CREATE_FUNCTION_COMPILED);
  }

  @ParameterizedTest
  @MethodSource("textsToTest")
  @DisplayName("Parameterized - sql create statements tests")
  void test(String text) {
    UseCaseEngine.runTest(text, List.of(), Map.of());
  }
}
