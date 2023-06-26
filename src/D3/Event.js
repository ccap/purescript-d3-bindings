export const _setEventConst = event => eventHandler => selection => () =>
  selection.on(event, (e) => eventHandler(e)());


export const _setEvent = event => eventHandler => selection => () =>
  selection.on(event, (e, d) => eventHandler(e)(d)());

export const _setEventI = event => eventHandler => selection => () =>
  selection.on(event, (e, d, i) => eventHandler(e)(d)(i)());
