import * as d3 from 'd3';

export const _bindDataLoopStack = selection => () => selection.data(function (d) { return d });

export const _stackData = keys => data => () => d3.stack().keys(keys)(data);

export const _stackData0 = stackData => stackData[0]

export const _stackData1 = stackData => stackData[1]

export const _stackResultData = stackResult => stackResult;

export const _stackResultIndex = stackResult => stackResult.index;

export const _stackResultKey = stackResult => stackResult.key;
