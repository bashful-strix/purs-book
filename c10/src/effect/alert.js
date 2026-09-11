"use strict";

export const alert = msg => () =>
  window.alert(msg);

export const confirm = msg => () =>
  window.confirm(msg);
