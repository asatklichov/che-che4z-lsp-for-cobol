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

/** This test checks if sql RELEASE CONNECTION  statement works correctly. */
class TestSqlReleaseConnectionStatement {
  private static final String RELEASE_CONNECTION =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          // EXEC SQL RELEASE TOROLAB1;
          ;

  private static final String RELEASE_CONNECTION2 =
          "       IDENTIFICATION DIVISION.\n"
                  + "       PROGRAM-ID. HELLO-SQL.\n"
                  + "       DATA DIVISION.\n"
                  + "       WORKING-STORAGE SECTION.\n"
          // EXEC SQL RELEASE CURRENT;
          ;


  private static Stream<String> textsToTest() {
    // add all
    return Stream.of(RELEASE_CONNECTION, RELEASE_CONNECTION2);
  }

  @ParameterizedTest
  @MethodSource("textsToTest")
  @DisplayName("Parameterized - sql create statements tests")
  void test(String text) {
    UseCaseEngine.runTest(text, List.of(), Map.of());
  }
}
