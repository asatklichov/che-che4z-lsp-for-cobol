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

/** This test checks if sql INSERT statement works correctly. */
class TestSqlInsertStatement {
  private static final String INSERT1 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // INSERT INTO DSN8C10.EMP
      //     VALUES ('000205','MARY','T','SMITH','D11','2866',
      //              '1981-08-10','ANALYST',16,'F','1956-05-22',
      //             16345,500,2300);
      ;

  private static final String INSERT2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //  INSERT INTO SMITH.TEMPEMPL
      //     SELECT *
      //     FROM DSN8C10.EMP;
      ;

  private static final String INSERT3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // INSERT INTO SESSION.TEMPEMPL
      //     SELECT *
      //     FROM DSN8C10.EMP
      //     WHERE WORKDEPT='D11';
      ;

  private static final String INSERT4 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // INSERT INTO DSN8C10.EMP_PHOTO_RESUME(EMPNO, EMP_ROWID)
      //     VALUES (:HV_ENUM, DEFAULT);
      ;

  private static final String INSERT5 =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          // EXEC SQL INSERT INTO T1 FOR :hv ROWS VALUES (:hva:hvind) ATOMIC;
          ;

  private static Stream<String> textsToTest() {
    // add all
    return Stream.of(INSERT1, INSERT2, INSERT3, INSERT4, INSERT5);
  }

  @ParameterizedTest
  @MethodSource("textsToTest")
  @DisplayName("Parameterized - sql create statements tests")
  void test(String text) {
    UseCaseEngine.runTest(text, List.of(), Map.of());
  }
}
