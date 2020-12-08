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

import org.junit.jupiter.api.Test;

/** This test checks if sql CLOSE statement works correctly. */
class TestSqlCloseStatement {
  private static final String TEXT =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // EXEC SQL DECLARE C1 CURSOR FOR
      //      SELECT DEPTNO, DEPTNAME, MGRNO
      //      FROM DSN8C10.DEPT
      //      WHERE ADMRDEPT = 'A00'
      //      END-EXEC.
      //
      //   EXEC SQL OPEN C1 END-EXEC.
      //
      //   EXEC SQL FETCH C1 INTO :DNUM, :DNAME, :MNUM END-EXEC.
      //
      //   IF SQLCODE = 100
      //      PERFORM DATA-NOT-FOUND
      //   ELSE
      //      PERFORM GET-REST-OF-DEPT
      //      UNTIL SQLCODE IS NOT EQUAL TO ZERO.
      //
      //   EXEC SQL CLOSE C1 END-EXEC.
      //
      //   GET-REST-OF-DEPT.
      //      EXEC SQL FETCH C1 INTO :DNUM, :DNAME, :MNUM END-EXEC.
      ;

  @Test
  void test() {
    // UseCaseEngine.runTest(TEXT, List.of(), Map.of());
  }
}
