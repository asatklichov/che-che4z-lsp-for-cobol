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
 * This PARAMETERIZED test checks if all below sql DESCRIBE statements works correctly.
 *
 * <pre>
 * - DESCRIBE CURSOR
 * - DESCRIBE INPUT
 * - DESCRIBE OUTPUT
 * - DESCRIBE PROCEDURE
 * - DESCRIBE TABLE
 *
 * </pre>
 */
class TestSqlAllDescribeStatements {
  private static final String DESCRIBE_CURSOR =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  EXEC SQL DESCRIBE CURSOR C1 INTO :sqlda1
      ;

  private static final String DESCRIBE_INPUT =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //   /* STMT1_STR contains INSERT statement with VALUES clause  */
      //   EXEC SQL  PREPARE STMT1_NAME FROM :STMT1_STR;
      //  … /* code to set SQLN to 5 and to allocate the SQLDA         */
      //   EXEC SQL  DESCRIBE INPUT STMT1_NAME INTO :SQLDA;
      ;

  private static final String DESCRIBE_OUTPUT =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // EXEC SQL  BEGIN DECLARE SECTION;
      //    DCL  STMT1_STR   CHAR(200)  VARYING;
      //  EXEC SQL  END DECLARE SECTION;
      //  EXEC SQL  INCLUDE SQLDA;
      //  EXEC SQL  DECLARE DYN_CURSOR CURSOR FOR STMT1_NAME;
      //  … /* code to prompt user for a query, then to generate */
      //      /* a select-statement in the STMT1_STR            */
      //  EXEC SQL  PREPARE STMT1_NAME FROM :STMT1_STR;
      //  … /* code to set SQLN to zero and to allocate the SQLDA */
      //  EXEC SQL  DESCRIBE STMT1_NAME INTO :SQLDA;
      //  … /* code to check that SQLD is greater than zero, to set */
      //      /* SQLN to SQLD, then to re-allocate the SQLDA          */
      //  EXEC SQL  DESCRIBE STMT1_NAME INTO :SQLDA;
      //  … /* code to prepare for the use of the SQLDA             */
      //  EXEC SQL  OPEN DYN_CURSOR;
      //  … /* loop to fetch rows from result table                 */
      //  EXEC SQL  FETCH DYN_CURSOR USING DESCRIPTOR :SQLDA;
      ;

  private static final String DESCRIBE_PROCEDURE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  EXEC SQL CONNECT TO SITE2;
      //   EXEC SQL CALL P1;
      //   EXEC SQL DESCRIBE PROCEDURE P1 INTO :SQLDA1;
      ;

  private static final String DESCRIBE_TABLE =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  ?
      ;

  private static Stream<String> textsToTest() {
    // add all
    return Stream.of(
        DESCRIBE_CURSOR, DESCRIBE_INPUT, DESCRIBE_OUTPUT, DESCRIBE_PROCEDURE, DESCRIBE_TABLE);
  }

  @ParameterizedTest
  @MethodSource("textsToTest")
  @DisplayName("Parameterized - sql create statements tests")
  void test(String text) {
    UseCaseEngine.runTest(text, List.of(), Map.of());
  }
}
