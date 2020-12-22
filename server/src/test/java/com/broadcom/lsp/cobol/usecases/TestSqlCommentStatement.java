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

/** This test checks if sql COMMENT statement works correctly. */
class TestSqlCommentStatement {

  private static final String COMMENT1 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // COMMENT ON TABLE DSN8C10.EMP
      //     IS 'REFLECTS 1ST QTR 81 REORG';
      ;

  private static final String COMMENT2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // COMMENT ON COLUMN DSN8C10.DEPT.DEPTNO
      //     IS 'DEPARTMENT ID - UNIQUE';
      ;

  private static final String COMMENT3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // COMMENT ON FUNCTION CHEM.ATOMIC_WEIGHT
      //     IS 'TAKES ATOMIC NUMBER AND GIVES ATOMIC WEIGHT';
      ;

  private static final String COMMENT4 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // COMMENT ON PROCEDURE BIOLOGY.OSMOSIS
      //     IS 'CALCULATIONS THAT MODEL OSMOSIS';
      ;

  private static final String COMMENT5 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      // COMMENT ON ROLE ROLE1
      //     IS 'Role defined for trusted context, ctx1';
      ;

  private static Stream<String> textsToTest() {
    // add all
    return Stream.of(COMMENT1, COMMENT2, COMMENT3, COMMENT4, COMMENT5);
  }

  @ParameterizedTest
  @MethodSource("textsToTest")
  @DisplayName("Parameterized - sql comment statements tests")
  void test(String text) {
    UseCaseEngine.runTest(text, List.of(), Map.of());
  }
}
