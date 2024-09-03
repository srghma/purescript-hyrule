export const objHack = (tag) => () => {
  return {
    r: false, // isRunning
    q: [],    // queue
    m: [{}],  // subscriptions
    tag       // dubug tag
  };
};

export const insertObjHack = (k, v, o) => {
  o.m[o.m.length - 1][k] = v;
};

export const deleteObjHack = (k, o) => {
  for (const m of o.m) {
    if (delete m[k]) {
      return true;
    }
  }
  return false;
};

const run = (o, M, f, i) => {
  o.m.push({});
  for (const kv of Object.entries(o.m[i])) {
    const k = kv[0];
    const v = kv[1];
    f(v);
    if (Object.keys(o.m[i + 1]).length) run(o, M, f, i + 1);
    o.m[i + 1] = {};
    o.m.length = i + 1 + 1;
    M[k] = v;
  }
};
export const fastForeachOhE = (o, ff) => {
  let f = ff;
  while (true) {
    if (o.r) {
      o.q.push(f);
      return;
    }
    o.r = true;
    const M = {}; // new subscription object
    run(o, M, f, 0);
    o.m.length = 0; // make empty
    o.m.push(M);
    o.r = false;
    f = o.q.shift();
    if (f == undefined) {
      break;
    }
  }
};
