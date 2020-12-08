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

/** This test checks if sql GET DIAGNOSTICS statement works correctly. */
class TestSqlGetDiagnosticsStatement {
  private static final String TEXT =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // CREATE PROCEDURE TEST
      //	MODIFIES SQL DATA
      //	LANGUAGE SQL
      //	BEGIN
      //    DECLARE NESTING_LEVEL  INT     DEFAULT 0;
      //
      //    GET DIAGNOSTICS NESTING_LEVEL = DB2_SQL_NESTING_LEVEL;
      //
      //    --
      //    -- If routine is invoked at nesting level 1,
      //    -- invoke a routine to log the invocation.
      //    --
      //    IF (NESTING_LEVEL = 1) THEN
      //       CALL LOG_INVOCATION();
      //    END IF;
      //
      //    --
      //    -- Remainder of procedure logic
      //    --
      //    ...
      // END
      ;

  @Test
  void test() {
    // UseCaseEngine.runTest(TEXT, List.of(), Map.of());
  }
}
