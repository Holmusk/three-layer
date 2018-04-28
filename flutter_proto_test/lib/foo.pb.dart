///
//  Generated code. Do not modify.
///
// ignore_for_file: non_constant_identifier_names,library_prefixes
library foo;

// ignore: UNUSED_SHOWN_NAME
import 'dart:core' show int, bool, double, String, List, override;

import 'package:protobuf/protobuf.dart';

class SearchRequest extends GeneratedMessage {
  static final BuilderInfo _i = new BuilderInfo('SearchRequest')
    ..aOS(1, 'query')
    ..a<int>(2, 'pageNumber', PbFieldType.O3)
    ..a<int>(3, 'resultPerPage', PbFieldType.O3)
    ..hasRequiredFields = false
  ;

  SearchRequest() : super();
  SearchRequest.fromBuffer(List<int> i, [ExtensionRegistry r = ExtensionRegistry.EMPTY]) : super.fromBuffer(i, r);
  SearchRequest.fromJson(String i, [ExtensionRegistry r = ExtensionRegistry.EMPTY]) : super.fromJson(i, r);
  SearchRequest clone() => new SearchRequest()..mergeFromMessage(this);
  BuilderInfo get info_ => _i;
  static SearchRequest create() => new SearchRequest();
  static PbList<SearchRequest> createRepeated() => new PbList<SearchRequest>();
  static SearchRequest getDefault() {
    if (_defaultInstance == null) _defaultInstance = new _ReadonlySearchRequest();
    return _defaultInstance;
  }
  static SearchRequest _defaultInstance;
  static void $checkItem(SearchRequest v) {
    if (v is! SearchRequest) checkItemFailed(v, 'SearchRequest');
  }

  String get query => $_getS(0, '');
  set query(String v) { $_setString(0, v); }
  bool hasQuery() => $_has(0);
  void clearQuery() => clearField(1);

  int get pageNumber => $_get(1, 0);
  set pageNumber(int v) { $_setUnsignedInt32(1, v); }
  bool hasPageNumber() => $_has(1);
  void clearPageNumber() => clearField(2);

  int get resultPerPage => $_get(2, 0);
  set resultPerPage(int v) { $_setUnsignedInt32(2, v); }
  bool hasResultPerPage() => $_has(2);
  void clearResultPerPage() => clearField(3);
}

class _ReadonlySearchRequest extends SearchRequest with ReadonlyMessageMixin {}

class SearchResponse extends GeneratedMessage {
  static final BuilderInfo _i = new BuilderInfo('SearchResponse')
    ..pPS(1, 'response')
    ..hasRequiredFields = false
  ;

  SearchResponse() : super();
  SearchResponse.fromBuffer(List<int> i, [ExtensionRegistry r = ExtensionRegistry.EMPTY]) : super.fromBuffer(i, r);
  SearchResponse.fromJson(String i, [ExtensionRegistry r = ExtensionRegistry.EMPTY]) : super.fromJson(i, r);
  SearchResponse clone() => new SearchResponse()..mergeFromMessage(this);
  BuilderInfo get info_ => _i;
  static SearchResponse create() => new SearchResponse();
  static PbList<SearchResponse> createRepeated() => new PbList<SearchResponse>();
  static SearchResponse getDefault() {
    if (_defaultInstance == null) _defaultInstance = new _ReadonlySearchResponse();
    return _defaultInstance;
  }
  static SearchResponse _defaultInstance;
  static void $checkItem(SearchResponse v) {
    if (v is! SearchResponse) checkItemFailed(v, 'SearchResponse');
  }

  List<String> get response => $_getList(0);
}

class _ReadonlySearchResponse extends SearchResponse with ReadonlyMessageMixin {}

