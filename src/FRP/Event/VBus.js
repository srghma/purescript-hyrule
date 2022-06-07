const _____$__$_$$_vbus = "_____$__$_$$_vbus";

export function unsafeMarkAsVbus(a) {
	a[_____$__$_$$_vbus] = _____$__$_$$_vbus;
	return a;
}

export function unsafeDestroyS(s) {
	return () => {
		for (const key in s) {
			delete s[key];
		}
	};
}

export function unsafePE(u) {
	return () => {
		const doAssigns = (s, p, e, u) => {
			const ok = Object.keys(u);
			for (var i = 0; i < ok.length; i++) {
				if (
					u[ok[i]] instanceof Object &&
					u[ok[i]][_____$__$_$$_vbus] === _____$__$_$$_vbus
				) {
					const p0 = {};
					const e0 = {};
					doAssigns(s, p0, e0, u[ok[i]]);
					p[ok[i]] = p0;
					e[ok[i]] = e0;
				} else {
					const rn = `${Math.random()}`;
					s[rn] = {};
					p[ok[i]] = (v) => () => {
						const rnk = Object.values(s[rn]);
						for (var j = 0; j < rnk.length; j++) {
							rnk[j](v)();
						}
					};
					e[ok[i]] = (f) => () => {
						const k = `${Math.random()}`;
						s[rn][k] = f;
						return () => {
							delete s[rn][k];
						};
					};
				}
			}
		};
		const s = {};
		const p = {};
		const e = {};
		doAssigns(s, p, e, u);
		return { p: p, e: e, s: s };
	};
}
