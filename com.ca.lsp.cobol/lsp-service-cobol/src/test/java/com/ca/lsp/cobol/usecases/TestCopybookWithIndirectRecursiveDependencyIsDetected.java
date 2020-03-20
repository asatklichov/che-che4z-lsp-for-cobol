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
 *   Broadcom, Inc. - initial API and implementation
 */

package com.ca.lsp.cobol.usecases;

import com.ca.lsp.cobol.positive.CobolText;
import org.eclipse.lsp4j.Diagnostic;
import org.eclipse.lsp4j.Range;
import org.junit.Test;

import java.util.List;

import static java.util.Arrays.asList;
import static org.junit.Assert.assertEquals;

/**
 * This test checks the error shown if the copybook that is used in the Cobol file contains a link
 * to another one, that has a dependency to the first copybook.
 */
public class TestCopybookWithIndirectRecursiveDependencyIsDetected extends NegativeUseCase {

  private static final String TEXT =
      "        IDENTIFICATION DIVISION.\r\n"
          + "        PROGRAM-ID. test1.\r\n"
          + "        DATA DIVISION.\r\n"
          + "        WORKING-STORAGE SECTION.\r\n"
          + "        COPY INDIRECT-COPY.\n\n"
          + "        PROCEDURE DIVISION.\n\n";

  private static final String INDIRECT_COPY = "        COPY INNER-COPY.";
  private static final String INNER_COPY = "        COPY INDIRECT-COPY.";

  public TestCopybookWithIndirectRecursiveDependencyIsDetected() {
    super(TEXT);
  }

  @Override
  @Test
  public void test() {
    super.test(
        asList(
            new CobolText("INNER-COPY", INNER_COPY),
            new CobolText("INDIRECT-COPY", INDIRECT_COPY)));
  }

  @Override
  protected void assertDiagnostics(List<Diagnostic> diagnostics) {
    assertEquals("Number of diagnostics", 1, diagnostics.size());
    Diagnostic diagnostic = diagnostics.get(0);
    assertEquals("Recursive copybook declaration for: INDIRECT-COPY", diagnostic.getMessage());

    Range range = diagnostic.getRange();
    assertEquals("Diagnostic start line", 4, range.getStart().getLine());
    assertEquals("Diagnostic start character", 13, range.getStart().getCharacter());
    assertEquals("Diagnostic end line", 4, range.getEnd().getLine());
    assertEquals("Diagnostic end character", 26, range.getEnd().getCharacter());
  }
}
