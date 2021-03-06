/*
 * Copyright (c) 2020 Broadcom.
 *
 * The term "Broadcom" refers to Broadcom Inc. and/or its subsidiaries.
 *
 * This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License 2.0
 * which is available at https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 * Broadcom, Inc. - initial API and implementation
 *
 */
package com.broadcom.lsp.domain.cobol.event;

import com.broadcom.lsp.domain.cobol.event.model.FetchedCopybookEvent;
import com.broadcom.lsp.domain.cobol.event.model.RequiredCopybookEvent;
import com.broadcom.lsp.domain.cobol.event.model.RunAnalysisEvent;
import com.broadcom.lsp.domain.cobol.event.model.UnknownEvent;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

@Slf4j
class CopybookEventsTest {
  private static final String UNKNOWN_EVENT_MESSAGE = "Dummy event";
  private static final String REQCPY = "REQCPY";
  private static final String FETCHEDCPY = "FETCHEDCPY";
  private static final String UNKNOWN = "UNKNOWN";
  private static final String RUN_ANALYSIS_EVENT = "RUN_ANALYSIS";
  private static final String COPYBOOK_NAME = "Test";
  private static final String COPYBOOK_URI = "file:///C:/Users/test/Test.cpy";
  private static final String COBOL_FILE_URI = "file:///C:/Users/test/Main.cbl";

  private static final String COPYBOOK_CONTENT = "000000 IDENTIFICATION DIVISION.";
  private static final String COPYBOOK_ANALYSIS_ENABLED = "ENABLED";

  /** Test the RequiredCopybookEvent DTO is correctly populated */
  @Test
  void requestCopybookEventTest() {
    assertEquals(REQCPY, getRequireCopybookHeader());
  }

  /** Test the FetchedCopybookEvent DTO is correctly populated */
  @Test
  void fetchCopybookEventTest() {
    assertEquals(FETCHEDCPY, getFetchCopybookHeader());
  }

  /** Test the UnknownEvent DTO is correctly populated */
  @Test
  void unknownEventTest() {
    assertEquals(UNKNOWN, getUnknownCopybookHeader());
  }

  /** Test the RunAnalysisEvent DTO is correctly populated. */
  @Test
  void runAnalysisTest() {
    assertEquals(RUN_ANALYSIS_EVENT, getRunAnalysisHeader());
  }

  /** This test verifies that the header of an event object doesn't return a wrong type */
  @Test
  void negativeTestEvent() {
    assertNotEquals(FETCHEDCPY, getUnknownCopybookHeader());
    assertNotEquals(REQCPY, getUnknownCopybookHeader());
    assertNotEquals(RUN_ANALYSIS_EVENT, getUnknownCopybookHeader());

    assertNotEquals(FETCHEDCPY, getRequireCopybookHeader());
    assertNotEquals(UNKNOWN, getRequireCopybookHeader());
    assertNotEquals(RUN_ANALYSIS_EVENT, getRequireCopybookHeader());

    assertNotEquals(REQCPY, getFetchCopybookHeader());
    assertNotEquals(UNKNOWN, getFetchCopybookHeader());
    assertNotEquals(RUN_ANALYSIS_EVENT, getFetchCopybookHeader());
  }

  /** This test verifies that a null header not returned from the callee. */
  @Test
  void notNullValuesFromHeaderReturned() {
    // assert that header is not null value
    assertNotNull(getUnknownCopybookHeader());
    assertNotNull(getRequireCopybookHeader());
    assertNotNull(getFetchCopybookHeader());
    assertNotNull(getRunAnalysisHeader());
  }

  private String getRequireCopybookHeader() {
    return new RequiredCopybookEvent(COPYBOOK_NAME, COBOL_FILE_URI, COPYBOOK_ANALYSIS_ENABLED)
        .getHeader();
  }

  private String getFetchCopybookHeader() {
    return new FetchedCopybookEvent(COPYBOOK_NAME, COPYBOOK_URI, COPYBOOK_CONTENT).getHeader();
  }

  private String getUnknownCopybookHeader() {
    return new UnknownEvent(UNKNOWN_EVENT_MESSAGE).getHeader();
  }

  private String getRunAnalysisHeader() {
    return new RunAnalysisEvent().getHeader();
  }
}
