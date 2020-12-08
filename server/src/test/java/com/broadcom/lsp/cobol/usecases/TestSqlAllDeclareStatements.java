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

/**
 * This PARAMETERIZED test checks if all below DECLARE statements works correctly.
 *
 * <pre>
 * - DECLARE CURSOR
 * - DECLARE GLOBAL TEMPORARY TABLE
 * - DECLARE STATEMENT
 * - DECLARE TABLE
 * - DECLARE VARIABLE
 *
 * </pre>
 */
class TestSqlAllDeclareStatements {
  private static final String DECLARE_CURSOR =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // EXEC SQL DECLARE C1 CURSOR FOR
      //      SELECT DEPTNO, DEPTNAME, MGRNO
      //      FROM DSN8C10.DEPT
      //      WHERE ADMRDEPT = 'A00';
      ;

  private static final String DECLARE_GLOBAL_TEMP_TABLE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  DECLARE GLOBAL TEMPORARY TABLE SESSION.TEMP_EMP
      //      (EMPNO     CHAR(6)   NOT NULL,
      //       SALARY    DECIMAL(9, 2),
      //       BONUS     DECIMAL(9, 2),
      //       COMM      DECIMAL(9, 2))
      //       CCSID EBCDIC
      //       ON COMMIT PRESERVE ROWS;
      ;

  private static final String DECLARE_STATEMENT =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  EXEC SQL DECLARE OBJECT_STATEMENT STATEMENT;
      //
      //   EXEC SQL INCLUDE SQLDA;
      //   EXEC SQL DECLARE C1 CURSOR FOR OBJECT_STATEMENT;
      //
      //   ( SOURCE_STATEMENT IS "SELECT DEPTNO, DEPTNAME,
      //     MGRNO FROM DSN8C10.DEPT WHERE ADMRDEPT = 'A00'"  )
      //
      //   EXEC SQL PREPARE OBJECT_STATEMENT FROM SOURCE_STATEMENT;
      //   EXEC SQL DESCRIBE OBJECT_STATEMENT INTO SQLDA;
      //
      //   /* Examine SQLDA */
      //
      //   EXEC SQL OPEN C1;
      //
      //   DO WHILE (SQLCODE = 0);
      //     EXEC SQL FETCH C1 USING DESCRIPTOR SQLDA;
      //
      //   /* Print results */
      //
      //   END;
      //
      //   EXEC SQL CLOSE C1;
      ;

  private static final String DECLARE_TABLE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  EXEC SQL DECLARE DSN8C10.EMP TABLE
      //     (EMPNO     CHAR(6)     NOT NULL,
      //      FIRSTNME  VARCHAR(12) NOT NULL,
      //      MIDINIT   CHAR(1)     NOT NULL,
      //      LASTNAME  VARCHAR(15) NOT NULL,
      //      WORKDEPT  CHAR(3)             ,
      //      PHONENO   CHAR(4)             ,
      //      HIREDATE  DATE                ,
      //      JOB       CHAR(8)             ,
      //      EDLEVEL   SMALLINT            ,
      //      SEX       CHAR(1)             ,
      //      BIRTHDATE DATE                ,
      //      SALARY    DECIMAL(9,2)        ,
      //      BONUS     DECIMAL(9,2)        ,
      //      COMM      DECIMAL(9,2)        );
      ;

  private static final String DECLARE_VARIABLE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  EXEC SQL DECLARE DSN8C10.EMP TABLE
      //     (EMPNO     CHAR(6)     NOT NULL,
      //      FIRSTNME  VARCHAR(12) NOT NULL,
      //      MIDINIT   CHAR(1)     NOT NULL,
      //      LASTNAME  VARCHAR(15) NOT NULL,
      //      WORKDEPT  CHAR(3)             ,
      //      PHONENO   CHAR(4)             ,
      //      HIREDATE  DATE                ,
      //      JOB       CHAR(8)             ,
      //      EDLEVEL   SMALLINT            ,
      //      SEX       CHAR(1)             ,
      //      BIRTHDATE DATE                ,
      //      SALARY    DECIMAL(9,2)        ,
      //      BONUS     DECIMAL(9,2)        ,
      //      COMM      DECIMAL(9,2)        );
      ;

  private static Stream<String> textsToTest() {
    // add all
    return Stream.of(
        DECLARE_CURSOR,
        DECLARE_GLOBAL_TEMP_TABLE,
        DECLARE_STATEMENT,
        DECLARE_TABLE,
        DECLARE_VARIABLE);
  }

  @ParameterizedTest
  @MethodSource("textsToTest")
  @DisplayName("Parameterized - sql create statements tests")
  void test(String text) {
    UseCaseEngine.runTest(text, List.of(), Map.of());
  }
}
