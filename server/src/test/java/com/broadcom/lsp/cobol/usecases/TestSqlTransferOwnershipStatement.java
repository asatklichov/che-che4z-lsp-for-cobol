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

/** This test checks if sql TRANSFER OWNERSHIP  statement works correctly. */
class TestSqlTransferOwnershipStatement {
  private static final String TRANSFER_OWNERSHIP1 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //     TRANSFER OWNERSHIP OF DATABASE DBCC001 TO USER USRT001
      //        REVOKE PRIVILEGES;
      ;

  private static final String TRANSFER_OWNERSHIP2 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //     TRANSFER OWNERSHIP OF DATABASE DBCC002 TO ROLE OWNRROLE
      //        REVOKE PRIVILEGES;
      ;

  private static final String TRANSFER_OWNERSHIP3 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //     TRANSFER OWNERSHIP OF DATABASE DBCC003 TO SESSION_USER
      //        REVOKE PRIVILEGES;
      ;

  private static final String TRANSFER_OWNERSHIP4 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //     TRANSFER OWNERSHIP OF TABLE EMPLOYEE.DEPT TO ROLE TBOWNR_ROLE
      //        REVOKE PRIVILEGES;
      ;

  private static final String TRANSFER_OWNERSHIP5 =
      "       IDENTIFICATION DIVISION.\n"
          + "       PROGRAM-ID. HELLO-SQL.\n"
          + "       DATA DIVISION.\n"
          + "       WORKING-STORAGE SECTION.\n"
      //     TRANSFER OWNERSHIP OF INDEX EMPLOYEE.SALARYIX TO USER IXOWNER
      //        REVOKE PRIVILEGES;
      ;


  private static Stream<String> textsToTest() {
    // add all
    return Stream.of(TRANSFER_OWNERSHIP1, TRANSFER_OWNERSHIP2, TRANSFER_OWNERSHIP3, TRANSFER_OWNERSHIP4, TRANSFER_OWNERSHIP5);
  }

  @ParameterizedTest
  @MethodSource("textsToTest")
  @DisplayName("Parameterized - sql create statements tests")
  void test(String text) {
    UseCaseEngine.runTest(text, List.of(), Map.of());
  }
}