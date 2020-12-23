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
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

/**
 * This PARAMETERIZED test checks if all below sql ALTER statements works correctly.
 *
 * <pre>
 * - ALTER DATABASE
 * - ALTER FUNCTION
 * - ALTER FUNCTION (compiled SQL scalar)
 * - ALTER FUNCTION (inlined SQL scalar)
 * - ALTER FUNCTION (SQL table)
 * - ALTER INDEX
 * - ALTER MASK
 * - ALTER PERMISSION
 * - ALTER PROCEDURE (external)
 * - ALTER PROCEDURE (sql native)
 * - ALTER SEQUENCE
 * - ALTER STOGROUP
 * - ALTER TABLE
 * - ALTER TABLESPACE
 * - ALTER TRIGGER (advanced)
 * - ALTER TRIGGER (basic)
 * - ALTER TRUSTED CONTEXT
 * - ALTER VIEW
 * </pre>
 */
class TestSqlAllAlterStatements {

  // ALTER: DATABASE
  private static final String ALTER_DB =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
          + "       EXEC SQL ALTER DATABASE ABCDE BUFFERPOOL BP2\n"
          + "       INDEXBP BP2 END-EXEC.";
  // ALTER FUNCTION statement
  private static final String ALTER_FUNCTION_EXT =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
          + "       EXEC SQL ALTER SPECIFIC FUNCTION ENGLES.FOCUS1 \n"
          + "       WLM ENVIRONMENT WLMENVNAME2 END-EXEC.";
  private static final String ALTER_FUNCTION_EXT2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
          + "       EXEC SQL ALTER FUNCTION ENGLES.CENTER (CHAR(25), DEC(5,2),\n"
          + "        INTEGER) RETURNS NULL ON NULL INPUT END-EXEC.";
  // ALTER FUNCTION (compiled SQL scalar)
  private static final String ALTER_FUNCTION_COMPILED =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
          + "       EXEC SQL ALTER FUNCTION MY_UDF1 DETERMINISTIC \n"
          + "       END-EXEC.";
  private static final String ALTER_FUNCTION_COMPILED2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
          + "       EXEC SQL ALTER FUNCTION REVERSE\n"
          + "       ALTER ACTIVE VERSION\n"
          + "       NOT DETERMINISTIC\n"
          + "       ALLOW DEBUG MODE END-EXEC.";
  // FYI: reverse is name of function

  private static final String ALTER_FUNCTION_COMPILED3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
          + "       EXEC SQL ALTER FUNCTION REVERSE(INSTR VARCHAR(4000))\n"
          + "       REPLACE VERSION V2 (INSTR VARCHAR(4000))\n"
          + "       RETURNS VARCHAR(4000)\n"
          + "       DETERMINISTIC\n"
          + "       NO EXTERNAL ACTION\n"
          + "       CONTAINS SQL\n"
          + "       \tBEGIN\n"
          + "       \tDECLARE REVSTR, RESTSTR VARCHAR(4000) DEFAULT '';\n"
          + "       \tDECLARE LEN INT;\n"
          + "       \tIF INSTR IS NULL THEN\n"
          + "       \t\tRETURN NULL;\n"
          + "       \tEND IF;\n"
          + "       \tSET RESTSTR = INSTR;\n"
          + "       \tSET LEN = LENGTH(INSTR);\n"
          + "       \tWHILE LEN > 0 DO\n"
          + "       \t\tSET (REVSTR, RESTSTR, LEN) = (SUBSTR(RESTSTR, 1, 1) CONCAT\n"
          + "       \t\t\t\tREVSTR, SUBSTR(RESTSTR, 2, LEN - 1), LEN - 1);\n"
          + "       \tEND WHILE;\n"
          + "       \tRETURN REVSTR;\n"
          + "        END-EXEC.";

  private static final String ALTER_FUNCTION_COMPILED4 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER FUNCTION REVERSE(INSTR VARCHAR(4000))
      //		ADD VERSION V3 (INSTR VARCHAR(4000))
      //		RETURNS VARCHAR(4000)
      //		DETERMINISTIC
      //		NO EXTERNAL ACTION
      //		CONTAINS SQL
      //	 	BEGIN
      //			DECLARE REVSTR, RESTSTR VARCHAR(4000) DEFAULT '';
      //			DECLARE LEN INT;
      //			IF INSTR IS NULL THEN
      //				RETURN NULL;
      //			END IF;
      //			SET (RESRSTR, LEN) = (INSTR, LENGTH(INSTR));
      //			WHILE LEN > 0 DO
      //				SET (REVSTR, RESTSTR, LEN) = (SUBSTR(RESTSTR, 1, 1) CONCAT
      //						REVSTR, SUBSTR(RESTSTR, 2, LEN - 1), LEN - 1);
      //			END WHILE;
      //			RETURN REVSTR;
      // 	 	END
      ;

  private static final String ALTER_FUNCTION_COMPILED5 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER FUNCTION REVERSE(INSTR VARCHAR(4000))
      //		ACTIVATE VERSION V3;
      ;

  private static final String ALTER_FUNCTION_COMPILED6 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER FUNCTION REVERSE(INSTR VARCHAR(4000))
      //		REGENERATE ACTIVE VERSION;
      ;

  // ALTER FUNCTION (inlined SQL scalar)
  private static final String ALTER_FUNCTION_INLINED =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER FUNCTION MY_UDF1
      //		DETERMINISTIC;
      ;

  private static final String ALTER_FUNCTION_SQL_TABLE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER FUNCTION GET_TABLE
      //	RESTRICT CARDINALITY 10000;
      ;
  // ALTER INDEX
  private static final String ALTER_INDEX =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER INDEX DSN8C10.XEMP1
      //     CLOSE NO;
      ;
  private static final String ALTER_INDEX2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER INDEX DSN8C10.XPROJ1
      //     BUFFERPOOL BP1
      //     COPY YES
      //     PIECESIZE 8M;
      ;
  private static final String ALTER_INDEX3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER INDEX X1
      //     NOT PADDED;
      ;
  private static final String ALTER_INDEX4 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER INDEX DSN8C10.XDEPT1
      //      BUFFERPOOL BP1
      //      CLOSE YES
      //      COPY YES
      //      USING VCAT CATLGG
      //      FREEPAGE 6
      //      PCTFREE 11
      //      GBPCACHE ALL
      //      ALTER PARTITION 3
      //         USING VCAT CATLGG
      //         FREEPAGE 13
      //         PCTFREE 13,
      //      ALTER PARTITION 4
      //         USING VCAT CATLGG
      //         GBPCACHE CHANGED,
      //      ALTER PARTITION 5
      //         USING VCAT CATLGG
      //         FREEPAGE 25
      //         PCTFREE 25;
      ;
  // ALTER MASK
  private static final String ALTER_MASK =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER MASK M1 ENABLE;
      ;
  private static final String ALTER_MASK2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER MASK M1 REGENERATE;
      //  COMMIT;
      ;
  // ALTER PERMISSION
  private static final String ALTER_PERMISSION =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER PERMISSION P1 ENABLE;
      ;
  private static final String ALTER_PERMISSION2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER PERMISSION P1 REGENERATE;
      ;
  // ALTER PROCEDURE external
  private static final String ALTER_PROCEDURE_EXT =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER PROCEDURE SYSPROC.MYPROC WLM ENVIRONMENT PARTSEC;
      ;
  // ALTER PROCEDURE SQL external
  private static final String ALTER_PROCEDURE_SQL_NATIVE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER PROCEDURE UPDATE_SALARY_1
      //      ALTER ACTIVE VERSION
      //      NOT DETERMINISTIC
      //      CALLED ON NULL INPUT
      //      ALLOW DEBUG MODE
      //      ASUTIME LIMIT 10
      ;
  private static final String ALTER_PROCEDURE_SQL_NATIVE2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER PROCEDURE UPDATE_SALARY_1
      //      REPLACE VERSION V2 (P1 INTEGER, P2 CHAR(5))
      //      MODIFIES SQL DATA
      //      UPDATE EMP SET SALARY = SALARY * RATE
      //            WHERE EMPNO = EMPLOYEE_NUMBER;
      ;
  private static final String ALTER_PROCEDURE_SQL_NATIVE3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER PROCEDURE UPDATE_SALARY_1
      //      ADD VERSION V3 (P1 INTEGER, P2 CHAR(5))
      //        UPDATE EMP SET SALARY = SALARY * (RATE*10)
      //            WHERE EMPNO = EMPLOYEE_NUMBER;
      ;
  private static final String ALTER_PROCEDURE_SQL_NATIVE4 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER PROCEDURE UPDATE_SALARY_1
      //      ACTIVATE VERSION V3;
      ;
  private static final String ALTER_PROCEDURE_SQL_NATIVE5 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER PROCEDURE UPDATE_SALARY_1
      //      REGENERATE ACTIVE VERSION;
      ;
  // ALTER SEQUENCE
  private static final String ALTER_SEQUENCE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER SEQUENCE org_seq RESTART;
      ;
  // ALTER STOGROUP
  private static final String ALTER_STOGROUP =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER STOGROUP DSN8G120
      //       ADD VOLUMES (DSNV04,DSNV05);
      ;
  private static final String ALTER_STOGROUP2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER STOGROUP DSN8G120
      //     REMOVE VOLUMES (DSNV04,DSNV05);
      ;
  private static final String ALTER_STOGROUP3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER STOGROUP DSNCG120
      //     NO KEY LABEL;
      ;
  // ALTER TABLE
  private static final String ALTER_TABLE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER TABLE DSN8C10.DEPT
      //     ALTER COLUMN DEPTNAME SET DATA TYPE VARCHAR(50)
      //     ADD BLDG CHAR(3) FOR SBCS DATA;
      ;
  private static final String ALTER_TABLE2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER TABLE DSN8C10.EMP
      //     VALIDPROC DSN8EAEM;
      ;
  private static final String ALTER_TABLE3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER TABLE DSN8C10.EMP
      //     VALIDPROC NULL;
      ;
  private static final String ALTER_TABLE4 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER TABLE DSN8C10.DEPT
      //     FOREIGN KEY(ADMRDEPT) REFERENCES DSN8C10.DEPT ON DELETE CASCADE;
      ;
  private static final String ALTER_TABLE5 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER TABLE DSN8C10.EMP
      //     ADD CHECK (SALARY >= 10000);
      ;
  private static final String ALTER_TABLE6 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER TABLE PRODINFO
      //     FOREIGN KEY (PRODNAME,PRODVERNO)
      //       REFERENCES PRODVER_1 (VERNAME,RELNO) ON DELETE RESTRICT;
      ;
  private static final String ALTER_TABLE7 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  ALTER TABLE DSN8C10.DEPT
      //     ADD CONSTRAINT KEY_DEPTNAME UNIQUE( DEPTNAME );
      ;
  private static final String ALTER_TABLE8 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  ALTER TABLE TRANSCOUNT ADD MATERIALIZED QUERY
      //     (SELECT ACCTID, LOCID, YEAR, COUNT(*) as cnt
      //      FROM TRANS
      //      GROUP BY ACCTID, LOCID, YEAR )
      //      DATA INITIALLY DEFERRED
      //      REFRESH DEFERRED
      //      MAINTAINED BY USER;
      ;
  private static final String ALTER_TABLE9 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  ALTER TABLE TB1
      //		ALTER COLUMN COL1
      //			SET DATA TYPE BINARY(6);
      ;
  private static final String ALTER_TABLE10 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER TABLE DSN8C10.EMP
      //             KEY LABEL SECUREKEY01;
      ;
  // ALTER TABLESPACE
  private static final String ALTER_TABLESPACE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  ALTER TABLESPACE DSN8D12A.DSN8S12D
      //     BUFFERPOOL BP2
      //     LOCKSIZE PAGE;
      ;
  private static final String ALTER_TABLESPACE2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER TABLESPACE DSN8D12A.DSN8S12E
      //     CLOSE NO
      //     SECQTY -1
      //     ALTER PARTITION 1 PCTFREE 20;
      ;
  private static final String ALTER_TABLESPACE3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER TABLESPACE TS01DB.TS01TS
      //  MAXPARTITIONS 30;
      ;
  // ALTER TRIGGER ADVANCED
  private static final String ALTER_TRIGGER_ADV =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER TRIGGER TRIGGER1
      // SECURED;
      ;
  private static final String ALTER_TRIGGER_ADV2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER TRIGGER TRIGGER1
      // ALTER ALLOW DEBUG MODE;
      ;
  private static final String ALTER_TRIGGER_ADV3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER TRIGGER TRIGGER1
      // ACTIVATE VERSION V3;
      ;
  private static final String ALTER_TRIGGER_ADV4 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER TRIGGER TRIGGER1
      // REGENERATE ACTIVE VERSION;
      ;
  // ALTER TRIGGER BASIC
  private static final String ALTER_TRIGGER =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER TRIGGER TRIGGER1
      // SECURED;
      ;
  // ALTER TRUSTED CONTEXT
  private static final String ALTER_TRUSTED_CONTEXT =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER TRUSTED CONTEXT CTX1
      //       ALTER DEFAULT ROLE CTXROLE2;
      ;
  private static final String ALTER_TRUSTED_CONTEXT2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // ALTER TRUSTED CONTEXT CTX3
      //        DISABLE
      //        ADD USE FOR BILL;
      ;
  private static final String ALTER_TRUSTED_CONTEXT3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  ALTER TRUSTED CONTEXT CTX4
      //      REPLACE USE FOR JOE WITHOUT AUTHENTICATION
      //      ADD USE FOR PUBLIC WITH AUTHENTICATION,
      //      TOM ROLE SPLROLE;
      ;
  private static final String ALTER_TRUSTED_CONTEXT4 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  ALTER TRUSTED CONTEXT REMOTECTX
      //      ALTER ATTRIBUTES (ADDRESS '9.12.155.200',
      //                        ENCRYPTION 'LOW');
      ;
  // ALTER VIEW
  private static final String ALTER_VIEW =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   ALTER VIEW MYVIEW REGENERATE;
      ;
  private static final String ALTER_VIEW2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  ALTER TRUSTED CONTEXT REMOTECTX
      //      ALTER ATTRIBUTES (ADDRESS '9.12.155.200',
      //                        ENCRYPTION 'LOW');
      ;

  private static Stream<String> textsToTest() {
    // add all
    return Stream.of(ALTER_DB, ALTER_FUNCTION_EXT, ALTER_FUNCTION_EXT2, ALTER_FUNCTION_COMPILED);
  }

  @Test
  void testAlterDatabase() {
    UseCaseEngine.runTest(ALTER_DB, List.of(), Map.of());
  }

  @Test
  void testAlterFunction() {
    UseCaseEngine.runTest(ALTER_FUNCTION_EXT, List.of(), Map.of());
  }

  @Test
  void testAlterFunctionExt2() {
    UseCaseEngine.runTest(ALTER_FUNCTION_EXT2, List.of(), Map.of());
  }

  // ALTER_FUNCTION_COMPILED
  @Test
  void testAlterFunctionCompiled() {
    UseCaseEngine.runTest(ALTER_FUNCTION_COMPILED, List.of(), Map.of());
  }

  // ALTER_FUNCTION_COMPILED2
  @Test
  void testAlterFunctionCompiled2() {
    UseCaseEngine.runTest(ALTER_FUNCTION_COMPILED2, List.of(), Map.of());
  }

  // ALTER_FUNCTION_COMPILED2
  @Test
  void testAlterFunctionCompiled3() {
    UseCaseEngine.runTest(ALTER_FUNCTION_COMPILED3, List.of(), Map.of());
  }

  @ParameterizedTest
  @MethodSource("textsToTest")
  @DisplayName("Parameterized - sql alter statements tests")
  void test(String text) {
    UseCaseEngine.runTest(text, List.of(), Map.of());
  }
}
