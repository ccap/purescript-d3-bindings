export const _setAttributeConst = attr => value => selection => () => selection.attr(attr, value);

export const _setAttribute = attr => toValue => selection => () =>
  selection.attr(attr, (d) => toValue(d));

export const _setAttributeI = attr => toValue => selection => () => selection.attr(attr, (d, i) => toValue(d)(i));
