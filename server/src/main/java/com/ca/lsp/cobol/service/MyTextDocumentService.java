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
 *
 *   Broadcom, Inc. - initial API and implementation
 */
package com.ca.lsp.cobol.service;

import com.broadcom.lsp.domain.cobol.databus.api.DataBusBroker;
import com.broadcom.lsp.domain.cobol.event.api.EventObserver;
import com.broadcom.lsp.domain.cobol.event.model.AnalysisFinishedEvent;
import com.broadcom.lsp.domain.cobol.event.model.DataEventType;
import com.broadcom.lsp.domain.cobol.event.model.RunAnalysisEvent;
import com.ca.lsp.cobol.service.delegates.actions.CodeActions;
import com.ca.lsp.cobol.service.delegates.communications.Communications;
import com.ca.lsp.cobol.service.delegates.completions.Completions;
import com.ca.lsp.cobol.service.delegates.formations.Formations;
import com.ca.lsp.cobol.service.delegates.references.Occurrences;
import com.ca.lsp.cobol.service.delegates.validations.AnalysisResult;
import com.ca.lsp.cobol.service.delegates.validations.LanguageEngineFacade;
import com.google.inject.Inject;
import com.google.inject.Singleton;
import lombok.Builder;
import lombok.extern.slf4j.Slf4j;
import org.eclipse.lsp4j.*;
import org.eclipse.lsp4j.jsonrpc.messages.Either;
import org.eclipse.lsp4j.services.TextDocumentService;

import javax.annotation.Nonnull;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.BiConsumer;

import static com.ca.lsp.cobol.service.CopybookProcessingMode.getCopybookProcessingMode;
import static com.ca.lsp.cobol.service.TextDocumentSyncType.DID_CHANGE;
import static com.ca.lsp.cobol.service.TextDocumentSyncType.DID_OPEN;
import static java.lang.String.format;
import static java.util.Collections.emptyList;
import static java.util.Optional.ofNullable;
import static java.util.concurrent.CompletableFuture.runAsync;
import static java.util.concurrent.CompletableFuture.supplyAsync;
import static java.util.stream.Collectors.toList;

/**
 * This class is a set of end-points to apply text operations for COBOL documents. All the requests
 * that start with "textDocument" go here. The current implementation contains only supported
 * language features. For more details, please, see the specification:
 * https://microsoft.github.io//language-server-protocol/specifications/specification-3-14/
 *
 * <p>For the maintainers: Please, add logging for exceptions if you run any asynchronous operation.
 * Also, you perform any communication with the client, do it a using {@link Communications}
 * instance.
 */
@Slf4j
@Singleton
public class MyTextDocumentService implements TextDocumentService, EventObserver<RunAnalysisEvent> {
  private static final List<String> COBOL_IDS = Arrays.asList("cobol", "cbl", "cob");
  private static final String GIT_FS_URI = "gitfs:/";
  private static final String GITFS_URI_NOT_SUPPORTED = "GITFS URI not supported";

  private final Map<String, MyDocumentModel> docs = new ConcurrentHashMap<>();
  private final Map<String, CompletableFuture<List<DocumentSymbol>>> outlineMap = new ConcurrentHashMap<>();

  private Communications communications;
  private LanguageEngineFacade engine;
  private Formations formations;
  private Completions completions;
  private Occurrences occurrences;
  private CodeActions actions;
  private DataBusBroker dataBus;

  @Inject
  @Builder
  MyTextDocumentService(
      Communications communications,
      LanguageEngineFacade engine,
      Formations formations,
      Completions completions,
      Occurrences occurrences,
      DataBusBroker dataBus,
      CodeActions actions) {
    this.communications = communications;
    this.engine = engine;
    this.formations = formations;
    this.completions = completions;
    this.occurrences = occurrences;
    this.actions = actions;
    this.dataBus = dataBus;

    dataBus.subscribe(DataEventType.RUN_ANALYSIS_EVENT, this);
  }

  Map<String, MyDocumentModel> getDocs() {
    return new HashMap<>(docs);
  }

  @Override
  public CompletableFuture<Either<List<CompletionItem>, CompletionList>> completion(
      CompletionParams params) {
    String uri = params.getTextDocument().getUri();
    return CompletableFuture.<Either<List<CompletionItem>, CompletionList>>supplyAsync(
            () -> Either.forRight(completions.collectFor(docs.get(uri), params)))
        .whenComplete(
            reportExceptionIfThrown(createDescriptiveErrorMessage("completion lookup", uri)));
  }

  @Override
  public CompletableFuture<CompletionItem> resolveCompletionItem(CompletionItem unresolved) {
    return supplyAsync(() -> completions.resolveDocumentationFor(unresolved))
        .whenComplete(
            reportExceptionIfThrown(
                createDescriptiveErrorMessage("completion resolving", unresolved.getLabel())));
  }

  @Override
  public CompletableFuture<List<? extends Location>> definition(
      TextDocumentPositionParams position) {
    String uri = position.getTextDocument().getUri();
    return CompletableFuture.<List<? extends Location>>supplyAsync(
            () -> occurrences.findDefinitions(docs.get(uri), position))
        .whenComplete(
            reportExceptionIfThrown(createDescriptiveErrorMessage("definitions resolving", uri)));
  }

  @Override
  public CompletableFuture<List<? extends Location>> references(ReferenceParams params) {
    String uri = params.getTextDocument().getUri();
    return CompletableFuture.<List<? extends Location>>supplyAsync(
            () -> occurrences.findReferences(docs.get(uri), params, params.getContext()))
        .whenComplete(
            reportExceptionIfThrown(createDescriptiveErrorMessage("references resolving", uri)));
  }

  @Override
  public CompletableFuture<List<? extends DocumentHighlight>> documentHighlight(
      TextDocumentPositionParams position) {
    String uri = position.getTextDocument().getUri();
    return CompletableFuture.<List<? extends DocumentHighlight>>supplyAsync(
            () -> occurrences.findHighlights(docs.get(uri), position))
        .whenComplete(
            reportExceptionIfThrown(createDescriptiveErrorMessage("document highlighting", uri)));
  }

  @Override
  public CompletableFuture<List<? extends TextEdit>> formatting(DocumentFormattingParams params) {
    String uri = params.getTextDocument().getUri();
    MyDocumentModel model = docs.get(uri);
    return CompletableFuture.<List<? extends TextEdit>>supplyAsync(() -> formations.format(model))
        .whenComplete(reportExceptionIfThrown(createDescriptiveErrorMessage("formatting", uri)));
  }

  @Override
  public CompletableFuture<List<Either<Command, CodeAction>>> codeAction(CodeActionParams params) {
    return supplyAsync(() -> actions.collect(params))
        .whenComplete(
            reportExceptionIfThrown(
                createDescriptiveErrorMessage(
                    "code actions lookup", params.getTextDocument().getUri())));
  }

  @Override
  public void didOpen(DidOpenTextDocumentParams params) {
    String uri = params.getTextDocument().getUri();
    outlineMap.put(uri, new CompletableFuture<>());
    // git FS URIs are not currently supported
    if (uri.startsWith(GIT_FS_URI)) {
      LOG.warn(String.join(" ", GITFS_URI_NOT_SUPPORTED, uri));
    }

    String text = params.getTextDocument().getText();
    String langId = params.getTextDocument().getLanguageId();
    registerEngineAndAnalyze(uri, langId, text);
  }

  @Override
  public void didChange(DidChangeTextDocumentParams params) {
    String uri = params.getTextDocument().getUri();
    outlineMap.put(uri, new CompletableFuture<>());
    String text = params.getContentChanges().get(0).getText();
    String fileExtension = extractExtension(uri);
    if (fileExtension != null && isCobolFile(fileExtension)) {
      analyzeChanges(uri, text);
    }
  }

  @Override
  public void didClose(DidCloseTextDocumentParams params) {
    String uri = params.getTextDocument().getUri();
    LOG.info(String.format("Document closing invoked on URI %s", uri));
    communications.publishDiagnostics(Map.of(uri, List.of()));
    docs.remove(uri);
  }

  @Override
  public void didSave(DidSaveTextDocumentParams params) {
    LOG.info("Document saved...");
  }

  @Override
  public void observerCallback(@Nonnull RunAnalysisEvent event) {
    docs.forEach((key, value) -> analyzeDocumentFirstTime(key, value.getText()));
  }

  private void registerEngineAndAnalyze(String uri, String languageType, String text) {
    String fileExtension = extractExtension(uri);
    if (fileExtension != null && !isCobolFile(fileExtension)) {
      communications.notifyThatExtensionIsUnsupported(fileExtension);
    } else if (isCobolFile(languageType)) {
      communications.notifyThatLoadingInProgress(uri);
      analyzeDocumentFirstTime(uri, text);
    } else {
      communications.notifyThatEngineNotFound(languageType);
    }
  }

  private boolean isCobolFile(String identifier) {
    return COBOL_IDS.contains(identifier.toLowerCase());
  }

  private String extractExtension(String uri) {
    return ofNullable(uri)
        .filter(it -> it.indexOf('.') > -1)
        .map(it -> it.substring(it.lastIndexOf('.') + 1))
        .orElse(null);
  }

  private void analyzeDocumentFirstTime(String uri, String text) {
    registerDocument(uri, new MyDocumentModel(text, AnalysisResult.empty()));
    runAsync(
            () -> {
              AnalysisResult result =
                  engine.analyze(uri, text, getCopybookProcessingMode(uri, DID_OPEN));
              ofNullable(docs.get(uri)).ifPresent(doc -> doc.setAnalysisResult(result));
              publishResult(uri, result);
              outlineMap.get(uri).complete(result.getOutlineTree());
            })
        .whenComplete(reportExceptionIfThrown(createDescriptiveErrorMessage("analysis", uri)));
  }

  void analyzeChanges(String uri, String text) {
    runAsync(
            () -> {
              AnalysisResult result =
                  engine.analyze(uri, text, getCopybookProcessingMode(uri, DID_CHANGE));
              registerDocument(uri, new MyDocumentModel(text, result));
              communications.publishDiagnostics(result.getDiagnostics());
              outlineMap.get(uri).complete(result.getOutlineTree());
            })
        .whenComplete(reportExceptionIfThrown(createDescriptiveErrorMessage("analysis", uri)));
  }

  private void publishResult(String uri, AnalysisResult result) {
    notifyAnalysisFinished(uri, result.getCopybookUsages());
    communications.cancelProgressNotification(uri);
    communications.publishDiagnostics(result.getDiagnostics());
    if (result.getDiagnostics().isEmpty()) communications.notifyThatDocumentAnalysed(uri);
  }

  private void notifyAnalysisFinished(String uri, Map<String, List<Location>> copybooks) {
    dataBus.postData(
        AnalysisFinishedEvent.builder()
            .documentUri(uri)
            .copybookUris(
                ofNullable(copybooks).map(Map::values).orElse(emptyList()).stream()
                    .flatMap(List::stream)
                    .map(Location::getUri)
                    .distinct()
                    .collect(toList()))
            .build());
  }

  @Override
  public CompletableFuture<List<Either<SymbolInformation, DocumentSymbol>>> documentSymbol(DocumentSymbolParams params) {
    String uri = params.getTextDocument().getUri();
    return outlineMap.get(uri)
        .thenApply(documentSymbols -> documentSymbols.stream()
            .map(Either::<SymbolInformation, DocumentSymbol>forRight)
            .collect(toList()))
        .whenComplete(reportExceptionIfThrown(createDescriptiveErrorMessage("symbol analysis", uri)));
  }

  private void registerDocument(String uri, MyDocumentModel document) {
    docs.put(uri, document);
  }

  private String createDescriptiveErrorMessage(String action, String uri) {
    return format("An exception thrown while applying %s for %s:", action, uri);
  }

  private BiConsumer<Object, Throwable> reportExceptionIfThrown(String message) {
    return (res, ex) -> ofNullable(ex).ifPresent(it -> LOG.error(message, it));
  }
}
