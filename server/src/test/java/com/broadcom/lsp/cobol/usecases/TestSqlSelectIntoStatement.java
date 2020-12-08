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

/** This test checks if sql SELECT INTO statement works correctly. */
class TestSqlSelectIntoStatement {
  private static final String SELECT_INTO1 =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          //   EXEC SQL SELECT MAX(SALARY)
          //     INTO :MAXSALRY
          //     FROM DSN8C10.EMP;
          ;

  private static final String SELECT_INTO2 =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          //    EXEC SQL SELECT * INTO :EMPREC
          //     FROM DSN8C10.EMP
          //     WHERE EMPNO = '528671'
          //   END-EXEC.
          ;

  private static final String SELECT_INTO3 =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          //   EXEC SQL SELECT * INTO :EMPREC
          //     FROM DSN8C10.EMP
          //     WHERE EMPNO = '528671'
          //     WITH RS USE AND KEEP EXCLUSIVE LOCKS
          //   END-EXEC.
          ;

  private static final String SELECT_INTO4 =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          //SELECT INTCOL1 INTO MYINTARRAY1[INTCOL2+MYINTVAR+1]
          // FROM T1
          // WHERE INTCOL1 = MYINTARRAY1[INTCOL2] ;
          ;

  private static Stream<String> textsToTest() {
    // add all
    return Stream.of(SELECT_INTO2,SELECT_INTO3,SELECT_INTO4 );
  }

  @ParameterizedTest
  @MethodSource("textsToTest")
  @DisplayName("Parameterized - sql create statements tests")
  void test(String text) {
    UseCaseEngine.runTest(text, List.of(), Map.of());
  }
}


