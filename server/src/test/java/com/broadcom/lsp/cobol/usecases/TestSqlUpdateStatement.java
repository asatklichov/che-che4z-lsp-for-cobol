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

/** This test checks if sql UPDATE statement works correctly. */
class TestSqlUpdateStatement {

  private static final String UPDATE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   UPDATE DSN8C10.EMP
      //     SET PHONENO='3565'
      //     WHERE EMPNO='000190';
      ;

  private static final String UPDATE2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  UPDATE DSN8C10.EMP
      //    SET SALARY = SALARY + 100
      //    WHERE WORKDEPT = 'D11';
      ;

  private static final String UPDATE3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   UPDATE DSN8C10.EMP
      //     SET SALARY = NULL, BONUS = NULL, COMM = NULL
      //     WHERE EMPNO='000250';
      ;

  private static final String UPDATE4 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   UPDATE DSN8C10.EMP
      //     SET PROJSIZE = (SELECT COUNT(*)
      //                     FROM DSN8C10.PROJ
      //                     WHERE DEPTNO = 'E21')
      //     WHERE WORKDEPT = 'E21';
      ;

  private static final String UPDATE5 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   EXEC SQL UPDATE DSN8C10.EMP
      //     SET SALARY = 2 * SALARY
      //     WHERE CURRENT OF C1;
      ;

  private static final String UPDATE6 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   CREATE TABLE EMP1
      //     (EMP_ROWID    ROWID GENERATED ALWAYS,
      //      EMPNO        CHAR(6),
      //      NAME         CHAR(30),
      //      SALARY       DECIMAL(9,2),
      //      PICTURE      BLOB(250K),
      //      RESUME       CLOB(32K));
      //
      ;

  private static final String UPDATE7 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  EXEC SQL UPDATE EMP X
      //    SET SALARY = 1.10 * SALARY
      //    WHERE SALARY < (SELECT AVG(SALARY) FROM EMP Y
      //    WHERE X.JOBCODE = Y.JOBCODE);
      //
      ;

  private static final String UPDATE8 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  EXEC SQL UPDATE EMP T1
      //    SET SALARY = (SELECT AVG(T2.SALARY) FROM EMP T2)
      //    WHERE WORKDEPT = 'E11' AND
      //          SALARY < (SELECT AVG(T3.SALARY) FROM EMP T3);
      ;

  private static final String UPDATE9 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  EXEC SQL
      //    DECLARE C1 CURSOR FOR
      //      SELECT BONUS
      //      FROM DSN8710.EMP
      //      WHERE WORKDEPT = 'E12'
      //      FOR UPDATE OF BONUS;
      //  EXEC SQL
      //    UPDATE DSN8710.EMP
      //      SET BONUS = ( SELECT .10 * SALARY FROM DSN8710.EMP Y
      //                    WHERE EMPNO = Y.EMPNO )
      //      WHERE CURRENT OF C1;
      ;


  private static final String UPDATE10 =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          //EXEC SQL UPDATE T1 SET C1 = 5 WHERE CURRENT OF CS1;
          ;

  private static Stream<String> textsToTest() {
    // add all
    return Stream.of(UPDATE, UPDATE2, UPDATE3);
  }

  @ParameterizedTest
  @MethodSource("textsToTest")
  @DisplayName("Parameterized - sql update statements tests")
  void test(String text) {
    UseCaseEngine.runTest(text, List.of(), Map.of());
  }
}
