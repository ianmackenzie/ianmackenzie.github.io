(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.0/optimize for better performance and smaller assets.');


var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === elm$core$Basics$EQ ? 0 : ord === elm$core$Basics$LT ? -1 : 1;
	}));
});



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = elm$core$Set$toList(x);
		y = elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? elm$core$Basics$LT : n ? elm$core$Basics$GT : elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File === 'function' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[94m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return word
		? elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? elm$core$Maybe$Nothing
		: elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? elm$core$Maybe$Just(n) : elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? elm$core$Result$Ok(value)
		: (value instanceof String)
			? elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!elm$core$Result$isOk(result))
					{
						return elm$core$Result$Err(A2(elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return elm$core$Result$Ok(elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if (elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return elm$core$Result$Err(elm$json$Json$Decode$OneOf(elm$core$List$reverse(errors)));

		case 1:
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!elm$core$Result$isOk(result))
		{
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList === 'function' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2(elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	result = init(result.a);
	var model = result.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		result = A2(update, msg, model);
		stepper(model = result.a, viewMetadata);
		_Platform_dispatchEffects(managers, result.b, subscriptions(model));
	}

	_Platform_dispatchEffects(managers, result.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				p: bag.n,
				q: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.q)
		{
			x = temp.p(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		r: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		r: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2(elm$json$Json$Decode$map, func, handler.a)
				:
			A3(elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}


// CREATE

var _Regex_never = /.^/;

var _Regex_fromStringWith = F2(function(options, string)
{
	var flags = 'g';
	if (options.multiline) { flags += 'm'; }
	if (options.caseInsensitive) { flags += 'i'; }

	try
	{
		return elm$core$Maybe$Just(new RegExp(string, flags));
	}
	catch(error)
	{
		return elm$core$Maybe$Nothing;
	}
});


// USE

var _Regex_contains = F2(function(re, string)
{
	return string.match(re) !== null;
});


var _Regex_findAtMost = F3(function(n, re, str)
{
	var out = [];
	var number = 0;
	var string = str;
	var lastIndex = re.lastIndex;
	var prevLastIndex = -1;
	var result;
	while (number++ < n && (result = re.exec(string)))
	{
		if (prevLastIndex == re.lastIndex) break;
		var i = result.length - 1;
		var subs = new Array(i);
		while (i > 0)
		{
			var submatch = result[i];
			subs[--i] = submatch
				? elm$core$Maybe$Just(submatch)
				: elm$core$Maybe$Nothing;
		}
		out.push(A4(elm$regex$Regex$Match, result[0], result.index, number, _List_fromArray(subs)));
		prevLastIndex = re.lastIndex;
	}
	re.lastIndex = lastIndex;
	return _List_fromArray(out);
});


var _Regex_replaceAtMost = F4(function(n, re, replacer, string)
{
	var count = 0;
	function jsReplacer(match)
	{
		if (count++ >= n)
		{
			return match;
		}
		var i = arguments.length - 3;
		var submatches = new Array(i);
		while (i > 0)
		{
			var submatch = arguments[i];
			submatches[--i] = submatch
				? elm$core$Maybe$Just(submatch)
				: elm$core$Maybe$Nothing;
		}
		return replacer(A4(elm$regex$Regex$Match, match, arguments[arguments.length - 2], count, _List_fromArray(submatches)));
	}
	return string.replace(re, jsReplacer);
});

var _Regex_splitAtMost = F3(function(n, re, str)
{
	var string = str;
	var out = [];
	var start = re.lastIndex;
	var restoreLastIndex = re.lastIndex;
	while (n--)
	{
		var result = re.exec(string);
		if (!result) break;
		out.push(string.slice(start, result.index));
		start = re.lastIndex;
	}
	out.push(string.slice(start));
	re.lastIndex = restoreLastIndex;
	return _List_fromArray(out);
});

var _Regex_infinity = Infinity;


function _Url_percentEncode(string)
{
	return encodeURIComponent(string);
}

function _Url_percentDecode(string)
{
	try
	{
		return elm$core$Maybe$Just(decodeURIComponent(string));
	}
	catch (e)
	{
		return elm$core$Maybe$Nothing;
	}
}



// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? elm$browser$Browser$Internal(next)
							: elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return elm$core$Result$isOk(result) ? elm$core$Maybe$Just(result.a) : elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail(elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}
var author$project$Guide$Document$Document = function (a) {
	return {$: 'Document', a: a};
};
var author$project$Guide$Document$LargeScreen = {$: 'LargeScreen'};
var author$project$Guide$Document$SmallScreen = {$: 'SmallScreen'};
var author$project$Guide$Document$Title = function (a) {
	return {$: 'Title', a: a};
};
var author$project$Guide$Document$CodeBlockContext = {$: 'CodeBlockContext'};
var author$project$Guide$Document$CompiledBullets = function (a) {
	return {$: 'CompiledBullets', a: a};
};
var author$project$Guide$Document$Interactive = F2(
	function (a, b) {
		return {$: 'Interactive', a: a, b: b};
	});
var author$project$Guide$Document$ParagraphContext = {$: 'ParagraphContext'};
var author$project$Guide$Document$SectionContext = {$: 'SectionContext'};
var author$project$Guide$Document$Static = F2(
	function (a, b) {
		return {$: 'Static', a: a, b: b};
	});
var author$project$Guide$Document$SubsectionContext = {$: 'SubsectionContext'};
var author$project$Guide$Document$TitleContext = {$: 'TitleContext'};
var author$project$Guide$Document$largeScreenFontSizes = {body: 16, bodyCode: 16, section: 32, sectionCode: 30, subsection: 22, subsectionCode: 22, title: 48, titleCode: 44};
var author$project$Guide$Document$smallScreenFontSizes = {body: 14, bodyCode: 14, section: 22, sectionCode: 22, subsection: 18, subsectionCode: 18, title: 32, titleCode: 28};
var author$project$Guide$Document$fontSizes = function (screenType) {
	if (screenType.$ === 'LargeScreen') {
		return author$project$Guide$Document$largeScreenFontSizes;
	} else {
		return author$project$Guide$Document$smallScreenFontSizes;
	}
};
var elm$core$Basics$EQ = {$: 'EQ'};
var elm$core$Basics$GT = {$: 'GT'};
var elm$core$Basics$LT = {$: 'LT'};
var elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3(elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var elm$core$List$cons = _List_cons;
var elm$core$Dict$toList = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var elm$core$Dict$keys = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2(elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var elm$core$Set$toList = function (_n0) {
	var dict = _n0.a;
	return elm$core$Dict$keys(dict);
};
var elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var elm$core$Array$foldr = F3(
	function (func, baseCase, _n0) {
		var tree = _n0.c;
		var tail = _n0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3(elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3(elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			elm$core$Elm$JsArray$foldr,
			helper,
			A3(elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var elm$core$Array$toList = function (array) {
	return A3(elm$core$Array$foldr, elm$core$List$cons, _List_Nil, array);
};
var elm$core$Basics$fdiv = _Basics_fdiv;
var elm$core$Basics$toFloat = _Basics_toFloat;
var mdgriffith$elm_ui$Internal$Model$Rgba = F4(
	function (a, b, c, d) {
		return {$: 'Rgba', a: a, b: b, c: c, d: d};
	});
var mdgriffith$elm_ui$Element$rgb255 = F3(
	function (red, green, blue) {
		return A4(mdgriffith$elm_ui$Internal$Model$Rgba, red / 255, green / 255, blue / 255, 1);
	});
var author$project$Guide$Document$lightGrey = A3(mdgriffith$elm_ui$Element$rgb255, 238, 238, 238);
var elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var elm$core$Basics$gt = _Utils_gt;
var elm$core$Basics$sub = _Basics_sub;
var elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var mdgriffith$elm_ui$Internal$Flag$Flag = function (a) {
	return {$: 'Flag', a: a};
};
var mdgriffith$elm_ui$Internal$Flag$Second = function (a) {
	return {$: 'Second', a: a};
};
var mdgriffith$elm_ui$Internal$Flag$flag = function (i) {
	return (i > 31) ? mdgriffith$elm_ui$Internal$Flag$Second(1 << (i - 32)) : mdgriffith$elm_ui$Internal$Flag$Flag(1 << i);
};
var mdgriffith$elm_ui$Internal$Flag$fontFamily = mdgriffith$elm_ui$Internal$Flag$flag(5);
var mdgriffith$elm_ui$Internal$Model$FontFamily = F2(
	function (a, b) {
		return {$: 'FontFamily', a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Model$StyleClass = F2(
	function (a, b) {
		return {$: 'StyleClass', a: a, b: b};
	});
var elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var elm$core$Basics$append = _Utils_append;
var elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var elm$core$Maybe$Nothing = {$: 'Nothing'};
var elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var elm$core$String$toLower = _String_toLower;
var elm$core$String$words = _String_words;
var mdgriffith$elm_ui$Internal$Model$renderFontClassName = F2(
	function (font, current) {
		return _Utils_ap(
			current,
			function () {
				switch (font.$) {
					case 'Serif':
						return 'serif';
					case 'SansSerif':
						return 'sans-serif';
					case 'Monospace':
						return 'monospace';
					case 'Typeface':
						var name = font.a;
						return A2(
							elm$core$String$join,
							'-',
							elm$core$String$words(
								elm$core$String$toLower(name)));
					case 'ImportFont':
						var name = font.a;
						var url = font.b;
						return A2(
							elm$core$String$join,
							'-',
							elm$core$String$words(
								elm$core$String$toLower(name)));
					default:
						var name = font.a.name;
						return A2(
							elm$core$String$join,
							'-',
							elm$core$String$words(
								elm$core$String$toLower(name)));
				}
			}());
	});
var mdgriffith$elm_ui$Element$Font$family = function (families) {
	return A2(
		mdgriffith$elm_ui$Internal$Model$StyleClass,
		mdgriffith$elm_ui$Internal$Flag$fontFamily,
		A2(
			mdgriffith$elm_ui$Internal$Model$FontFamily,
			A3(elm$core$List$foldl, mdgriffith$elm_ui$Internal$Model$renderFontClassName, 'ff-', families),
			families));
};
var mdgriffith$elm_ui$Internal$Model$Monospace = {$: 'Monospace'};
var mdgriffith$elm_ui$Element$Font$monospace = mdgriffith$elm_ui$Internal$Model$Monospace;
var mdgriffith$elm_ui$Internal$Model$Typeface = function (a) {
	return {$: 'Typeface', a: a};
};
var mdgriffith$elm_ui$Element$Font$typeface = mdgriffith$elm_ui$Internal$Model$Typeface;
var author$project$Guide$Document$sourceCodePro = mdgriffith$elm_ui$Element$Font$family(
	_List_fromArray(
		[
			mdgriffith$elm_ui$Element$Font$typeface('Source Code Pro'),
			mdgriffith$elm_ui$Element$Font$monospace
		]));
var elm$core$Basics$add = _Basics_add;
var elm$core$List$reverse = function (list) {
	return A3(elm$core$List$foldl, elm$core$List$cons, _List_Nil, list);
};
var elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							elm$core$List$foldl,
							fn,
							acc,
							elm$core$List$reverse(r4)) : A4(elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4(elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var elm$core$String$lines = _String_lines;
var elm$core$String$trim = _String_trim;
var mdgriffith$elm_ui$Internal$Model$Height = function (a) {
	return {$: 'Height', a: a};
};
var mdgriffith$elm_ui$Element$height = mdgriffith$elm_ui$Internal$Model$Height;
var mdgriffith$elm_ui$Internal$Model$Content = {$: 'Content'};
var mdgriffith$elm_ui$Element$shrink = mdgriffith$elm_ui$Internal$Model$Content;
var mdgriffith$elm_ui$Internal$Model$Width = function (a) {
	return {$: 'Width', a: a};
};
var mdgriffith$elm_ui$Element$width = mdgriffith$elm_ui$Internal$Model$Width;
var mdgriffith$elm_ui$Internal$Model$Unkeyed = function (a) {
	return {$: 'Unkeyed', a: a};
};
var mdgriffith$elm_ui$Internal$Model$AsColumn = {$: 'AsColumn'};
var mdgriffith$elm_ui$Internal$Model$asColumn = mdgriffith$elm_ui$Internal$Model$AsColumn;
var mdgriffith$elm_ui$Internal$Model$Generic = {$: 'Generic'};
var mdgriffith$elm_ui$Internal$Model$div = mdgriffith$elm_ui$Internal$Model$Generic;
var mdgriffith$elm_ui$Internal$Flag$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Flag$none = A2(mdgriffith$elm_ui$Internal$Flag$Field, 0, 0);
var mdgriffith$elm_ui$Internal$Model$NoNearbyChildren = {$: 'NoNearbyChildren'};
var mdgriffith$elm_ui$Internal$Style$classes = {above: 'a', active: 'atv', alignBottom: 'ab', alignCenterX: 'cx', alignCenterY: 'cy', alignContainerBottom: 'acb', alignContainerCenterX: 'accx', alignContainerCenterY: 'accy', alignContainerRight: 'acr', alignLeft: 'al', alignRight: 'ar', alignTop: 'at', alignedHorizontally: 'ah', alignedVertically: 'av', any: 's', behind: 'bh', below: 'b', bold: 'w7', borderDashed: 'bd', borderDotted: 'bdt', borderNone: 'bn', borderSolid: 'bs', capturePointerEvents: 'cpe', clip: 'cp', clipX: 'cpx', clipY: 'cpy', column: 'c', container: 'ctr', contentBottom: 'cb', contentCenterX: 'ccx', contentCenterY: 'ccy', contentLeft: 'cl', contentRight: 'cr', contentTop: 'ct', cursorPointer: 'cptr', cursorText: 'ctxt', focus: 'fcs', fullSize: 'fs', grid: 'g', hasBehind: 'hbh', heightContent: 'hc', heightFill: 'hf', heightFillPortion: 'hfp', hover: 'hv', imageContainer: 'ic', inFront: 'fr', inputMultiline: 'iml', inputText: 'it', italic: 'i', nearby: 'nb', noTextSelection: 'notxt', onLeft: 'ol', onRight: 'or', opaque: 'oq', overflowHidden: 'oh', page: 'pg', paragraph: 'p', passPointerEvents: 'ppe', root: 'ui', row: 'r', scrollbars: 'sb', scrollbarsX: 'sbx', scrollbarsY: 'sby', seButton: 'sbt', single: 'e', sizeByCapital: 'cap', spaceEvenly: 'sev', strike: 'sk', text: 't', textCenter: 'tc', textExtraBold: 'w8', textExtraLight: 'w2', textHeavy: 'w9', textJustify: 'tj', textJustifyAll: 'tja', textLeft: 'tl', textLight: 'w3', textMedium: 'w5', textNormalWeight: 'w4', textRight: 'tr', textSemiBold: 'w6', textThin: 'w1', textUnitalicized: 'tun', transition: 'ts', transparent: 'clr', underline: 'u', widthContent: 'wc', widthExact: 'we', widthFill: 'wf', widthFillPortion: 'wfp', wrapped: 'wrp'};
var mdgriffith$elm_ui$Internal$Model$columnClass = mdgriffith$elm_ui$Internal$Style$classes.any + (' ' + mdgriffith$elm_ui$Internal$Style$classes.column);
var mdgriffith$elm_ui$Internal$Model$gridClass = mdgriffith$elm_ui$Internal$Style$classes.any + (' ' + mdgriffith$elm_ui$Internal$Style$classes.grid);
var mdgriffith$elm_ui$Internal$Model$pageClass = mdgriffith$elm_ui$Internal$Style$classes.any + (' ' + mdgriffith$elm_ui$Internal$Style$classes.page);
var mdgriffith$elm_ui$Internal$Model$paragraphClass = mdgriffith$elm_ui$Internal$Style$classes.any + (' ' + mdgriffith$elm_ui$Internal$Style$classes.paragraph);
var mdgriffith$elm_ui$Internal$Model$rowClass = mdgriffith$elm_ui$Internal$Style$classes.any + (' ' + mdgriffith$elm_ui$Internal$Style$classes.row);
var mdgriffith$elm_ui$Internal$Model$singleClass = mdgriffith$elm_ui$Internal$Style$classes.any + (' ' + mdgriffith$elm_ui$Internal$Style$classes.single);
var mdgriffith$elm_ui$Internal$Model$contextClasses = function (context) {
	switch (context.$) {
		case 'AsRow':
			return mdgriffith$elm_ui$Internal$Model$rowClass;
		case 'AsColumn':
			return mdgriffith$elm_ui$Internal$Model$columnClass;
		case 'AsEl':
			return mdgriffith$elm_ui$Internal$Model$singleClass;
		case 'AsGrid':
			return mdgriffith$elm_ui$Internal$Model$gridClass;
		case 'AsParagraph':
			return mdgriffith$elm_ui$Internal$Model$paragraphClass;
		default:
			return mdgriffith$elm_ui$Internal$Model$pageClass;
	}
};
var elm$core$Basics$eq = _Utils_equal;
var elm$core$Basics$False = {$: 'False'};
var elm$core$Basics$True = {$: 'True'};
var elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var mdgriffith$elm_ui$Internal$Model$Keyed = function (a) {
	return {$: 'Keyed', a: a};
};
var mdgriffith$elm_ui$Internal$Model$NoStyleSheet = {$: 'NoStyleSheet'};
var mdgriffith$elm_ui$Internal$Model$Styled = function (a) {
	return {$: 'Styled', a: a};
};
var mdgriffith$elm_ui$Internal$Model$Unstyled = function (a) {
	return {$: 'Unstyled', a: a};
};
var mdgriffith$elm_ui$Internal$Model$addChildren = F2(
	function (existing, nearbyChildren) {
		switch (nearbyChildren.$) {
			case 'NoNearbyChildren':
				return existing;
			case 'ChildrenBehind':
				var behind = nearbyChildren.a;
				return _Utils_ap(behind, existing);
			case 'ChildrenInFront':
				var inFront = nearbyChildren.a;
				return _Utils_ap(existing, inFront);
			default:
				var behind = nearbyChildren.a;
				var inFront = nearbyChildren.b;
				return _Utils_ap(
					behind,
					_Utils_ap(existing, inFront));
		}
	});
var mdgriffith$elm_ui$Internal$Model$addKeyedChildren = F3(
	function (key, existing, nearbyChildren) {
		switch (nearbyChildren.$) {
			case 'NoNearbyChildren':
				return existing;
			case 'ChildrenBehind':
				var behind = nearbyChildren.a;
				return _Utils_ap(
					A2(
						elm$core$List$map,
						function (x) {
							return _Utils_Tuple2(key, x);
						},
						behind),
					existing);
			case 'ChildrenInFront':
				var inFront = nearbyChildren.a;
				return _Utils_ap(
					existing,
					A2(
						elm$core$List$map,
						function (x) {
							return _Utils_Tuple2(key, x);
						},
						inFront));
			default:
				var behind = nearbyChildren.a;
				var inFront = nearbyChildren.b;
				return _Utils_ap(
					A2(
						elm$core$List$map,
						function (x) {
							return _Utils_Tuple2(key, x);
						},
						behind),
					_Utils_ap(
						existing,
						A2(
							elm$core$List$map,
							function (x) {
								return _Utils_Tuple2(key, x);
							},
							inFront)));
		}
	});
var mdgriffith$elm_ui$Internal$Model$AsEl = {$: 'AsEl'};
var mdgriffith$elm_ui$Internal$Model$asEl = mdgriffith$elm_ui$Internal$Model$AsEl;
var mdgriffith$elm_ui$Internal$Model$AsParagraph = {$: 'AsParagraph'};
var mdgriffith$elm_ui$Internal$Model$asParagraph = mdgriffith$elm_ui$Internal$Model$AsParagraph;
var elm$core$Basics$and = _Basics_and;
var elm$core$Basics$not = _Basics_not;
var elm$core$Basics$identity = function (x) {
	return x;
};
var elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var elm$core$Array$branchFactor = 32;
var elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var elm$core$Basics$ceiling = _Basics_ceiling;
var elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var elm$core$Array$shiftStep = elm$core$Basics$ceiling(
	A2(elm$core$Basics$logBase, 2, elm$core$Array$branchFactor));
var elm$core$Elm$JsArray$empty = _JsArray_empty;
var elm$core$Array$empty = A4(elm$core$Array$Array_elm_builtin, 0, elm$core$Array$shiftStep, elm$core$Elm$JsArray$empty, elm$core$Elm$JsArray$empty);
var elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _n0 = A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodes);
			var node = _n0.a;
			var remainingNodes = _n0.b;
			var newAcc = A2(
				elm$core$List$cons,
				elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var elm$core$Tuple$first = function (_n0) {
	var x = _n0.a;
	return x;
};
var elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = elm$core$Basics$ceiling(nodeListSize / elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2(elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var elm$core$Basics$floor = _Basics_floor;
var elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var elm$core$Basics$mul = _Basics_mul;
var elm$core$Elm$JsArray$length = _JsArray_length;
var elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.tail),
				elm$core$Array$shiftStep,
				elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * elm$core$Array$branchFactor;
			var depth = elm$core$Basics$floor(
				A2(elm$core$Basics$logBase, elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2(elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2(elm$core$Basics$max, 5, depth * elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var elm$core$Basics$idiv = _Basics_idiv;
var elm$core$Basics$lt = _Utils_lt;
var elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = elm$core$Array$Leaf(
					A3(elm$core$Elm$JsArray$initialize, elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2(elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var elm$core$Basics$le = _Utils_le;
var elm$core$Basics$remainderBy = _Basics_remainderBy;
var elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return elm$core$Array$empty;
		} else {
			var tailLen = len % elm$core$Array$branchFactor;
			var tail = A3(elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - elm$core$Array$branchFactor;
			return A5(elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var elm$core$Basics$or = _Basics_or;
var elm$core$Char$toCode = _Char_toCode;
var elm$core$Char$isLower = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var elm$core$Char$isUpper = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var elm$core$Char$isAlpha = function (_char) {
	return elm$core$Char$isLower(_char) || elm$core$Char$isUpper(_char);
};
var elm$core$Char$isDigit = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var elm$core$Char$isAlphaNum = function (_char) {
	return elm$core$Char$isLower(_char) || (elm$core$Char$isUpper(_char) || elm$core$Char$isDigit(_char));
};
var elm$core$List$length = function (xs) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (_n0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var elm$core$List$map2 = _List_map2;
var elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2(elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var elm$core$List$range = F2(
	function (lo, hi) {
		return A3(elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			elm$core$List$map2,
			f,
			A2(
				elm$core$List$range,
				0,
				elm$core$List$length(xs) - 1),
			xs);
	});
var elm$core$String$all = _String_all;
var elm$core$String$fromInt = _String_fromNumber;
var elm$core$String$uncons = _String_uncons;
var elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var elm$json$Json$Decode$indent = function (str) {
	return A2(
		elm$core$String$join,
		'\n    ',
		A2(elm$core$String$split, '\n', str));
};
var elm$json$Json$Encode$encode = _Json_encode;
var elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + (elm$core$String$fromInt(i + 1) + (') ' + elm$json$Json$Decode$indent(
			elm$json$Json$Decode$errorToString(error))));
	});
var elm$json$Json$Decode$errorToString = function (error) {
	return A2(elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _n1 = elm$core$String$uncons(f);
						if (_n1.$ === 'Nothing') {
							return false;
						} else {
							var _n2 = _n1.a;
							var _char = _n2.a;
							var rest = _n2.b;
							return elm$core$Char$isAlpha(_char) && A2(elm$core$String$all, elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + (elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									elm$core$String$join,
									'',
									elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										elm$core$String$join,
										'',
										elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + (elm$core$String$fromInt(
								elm$core$List$length(errors)) + ' ways:'));
							return A2(
								elm$core$String$join,
								'\n\n',
								A2(
									elm$core$List$cons,
									introduction,
									A2(elm$core$List$indexedMap, elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								elm$core$String$join,
								'',
								elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + (elm$json$Json$Decode$indent(
						A2(elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var elm$json$Json$Decode$map = _Json_map1;
var elm$json$Json$Decode$map2 = _Json_map2;
var elm$json$Json$Decode$succeed = _Json_succeed;
var elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var elm$html$Html$div = _VirtualDom_node('div');
var elm$html$Html$p = _VirtualDom_node('p');
var elm$html$Html$s = _VirtualDom_node('s');
var elm$html$Html$u = _VirtualDom_node('u');
var elm$json$Json$Encode$string = _Json_wrap;
var elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			elm$json$Json$Encode$string(string));
	});
var elm$html$Html$Attributes$class = elm$html$Html$Attributes$stringProperty('className');
var elm$virtual_dom$VirtualDom$keyedNode = function (tag) {
	return _VirtualDom_keyedNode(
		_VirtualDom_noScript(tag));
};
var elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var mdgriffith$elm_ui$Internal$Flag$alignBottom = mdgriffith$elm_ui$Internal$Flag$flag(41);
var mdgriffith$elm_ui$Internal$Flag$alignRight = mdgriffith$elm_ui$Internal$Flag$flag(40);
var mdgriffith$elm_ui$Internal$Flag$centerX = mdgriffith$elm_ui$Internal$Flag$flag(42);
var mdgriffith$elm_ui$Internal$Flag$centerY = mdgriffith$elm_ui$Internal$Flag$flag(43);
var mdgriffith$elm_ui$Internal$Flag$heightBetween = mdgriffith$elm_ui$Internal$Flag$flag(45);
var mdgriffith$elm_ui$Internal$Flag$heightFill = mdgriffith$elm_ui$Internal$Flag$flag(37);
var elm$core$Bitwise$and = _Bitwise_and;
var mdgriffith$elm_ui$Internal$Flag$present = F2(
	function (myFlag, _n0) {
		var fieldOne = _n0.a;
		var fieldTwo = _n0.b;
		if (myFlag.$ === 'Flag') {
			var first = myFlag.a;
			return _Utils_eq(first & fieldOne, first);
		} else {
			var second = myFlag.a;
			return _Utils_eq(second & fieldTwo, second);
		}
	});
var mdgriffith$elm_ui$Internal$Flag$widthBetween = mdgriffith$elm_ui$Internal$Flag$flag(44);
var mdgriffith$elm_ui$Internal$Flag$widthFill = mdgriffith$elm_ui$Internal$Flag$flag(39);
var elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var elm$core$Dict$empty = elm$core$Dict$RBEmpty_elm_builtin;
var elm$core$Set$Set_elm_builtin = function (a) {
	return {$: 'Set_elm_builtin', a: a};
};
var elm$core$Set$empty = elm$core$Set$Set_elm_builtin(elm$core$Dict$empty);
var elm$core$Tuple$second = function (_n0) {
	var y = _n0.b;
	return y;
};
var elm$core$Dict$Black = {$: 'Black'};
var elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var elm$core$Basics$compare = _Utils_compare;
var elm$core$Dict$Red = {$: 'Red'};
var elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _n1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _n3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Red,
					key,
					value,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _n5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _n6 = left.d;
				var _n7 = _n6.a;
				var llK = _n6.b;
				var llV = _n6.c;
				var llLeft = _n6.d;
				var llRight = _n6.e;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Red,
					lK,
					lV,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5(elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, key, value, elm$core$Dict$RBEmpty_elm_builtin, elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _n1 = A2(elm$core$Basics$compare, key, nKey);
			switch (_n1.$) {
				case 'LT':
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3(elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5(elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3(elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _n0 = A3(elm$core$Dict$insertHelp, key, value, dict);
		if ((_n0.$ === 'RBNode_elm_builtin') && (_n0.a.$ === 'Red')) {
			var _n1 = _n0.a;
			var k = _n0.b;
			var v = _n0.c;
			var l = _n0.d;
			var r = _n0.e;
			return A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _n0;
			return x;
		}
	});
var elm$core$Set$insert = F2(
	function (key, _n0) {
		var dict = _n0.a;
		return elm$core$Set$Set_elm_builtin(
			A3(elm$core$Dict$insert, key, _Utils_Tuple0, dict));
	});
var elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _n1 = A2(elm$core$Basics$compare, targetKey, key);
				switch (_n1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var elm$core$Dict$member = F2(
	function (key, dict) {
		var _n0 = A2(elm$core$Dict$get, key, dict);
		if (_n0.$ === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var elm$core$Set$member = F2(
	function (key, _n0) {
		var dict = _n0.a;
		return A2(elm$core$Dict$member, key, dict);
	});
var elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var mdgriffith$elm_ui$Internal$Model$lengthClassName = function (x) {
	switch (x.$) {
		case 'Px':
			var px = x.a;
			return elm$core$String$fromInt(px) + 'px';
		case 'Content':
			return 'auto';
		case 'Fill':
			var i = x.a;
			return elm$core$String$fromInt(i) + 'fr';
		case 'Min':
			var min = x.a;
			var len = x.b;
			return 'min' + (elm$core$String$fromInt(min) + mdgriffith$elm_ui$Internal$Model$lengthClassName(len));
		default:
			var max = x.a;
			var len = x.b;
			return 'max' + (elm$core$String$fromInt(max) + mdgriffith$elm_ui$Internal$Model$lengthClassName(len));
	}
};
var elm$core$Basics$round = _Basics_round;
var mdgriffith$elm_ui$Internal$Model$floatClass = function (x) {
	return elm$core$String$fromInt(
		elm$core$Basics$round(x * 255));
};
var mdgriffith$elm_ui$Internal$Model$transformClass = function (transform) {
	switch (transform.$) {
		case 'Untransformed':
			return elm$core$Maybe$Nothing;
		case 'Moved':
			var _n1 = transform.a;
			var x = _n1.a;
			var y = _n1.b;
			var z = _n1.c;
			return elm$core$Maybe$Just(
				'mv-' + (mdgriffith$elm_ui$Internal$Model$floatClass(x) + ('-' + (mdgriffith$elm_ui$Internal$Model$floatClass(y) + ('-' + mdgriffith$elm_ui$Internal$Model$floatClass(z))))));
		default:
			var _n2 = transform.a;
			var tx = _n2.a;
			var ty = _n2.b;
			var tz = _n2.c;
			var _n3 = transform.b;
			var sx = _n3.a;
			var sy = _n3.b;
			var sz = _n3.c;
			var _n4 = transform.c;
			var ox = _n4.a;
			var oy = _n4.b;
			var oz = _n4.c;
			var angle = transform.d;
			return elm$core$Maybe$Just(
				'tfrm-' + (mdgriffith$elm_ui$Internal$Model$floatClass(tx) + ('-' + (mdgriffith$elm_ui$Internal$Model$floatClass(ty) + ('-' + (mdgriffith$elm_ui$Internal$Model$floatClass(tz) + ('-' + (mdgriffith$elm_ui$Internal$Model$floatClass(sx) + ('-' + (mdgriffith$elm_ui$Internal$Model$floatClass(sy) + ('-' + (mdgriffith$elm_ui$Internal$Model$floatClass(sz) + ('-' + (mdgriffith$elm_ui$Internal$Model$floatClass(ox) + ('-' + (mdgriffith$elm_ui$Internal$Model$floatClass(oy) + ('-' + (mdgriffith$elm_ui$Internal$Model$floatClass(oz) + ('-' + mdgriffith$elm_ui$Internal$Model$floatClass(angle))))))))))))))))))));
	}
};
var mdgriffith$elm_ui$Internal$Model$getStyleName = function (style) {
	switch (style.$) {
		case 'Shadows':
			var name = style.a;
			return name;
		case 'Transparency':
			var name = style.a;
			var o = style.b;
			return name;
		case 'Style':
			var _class = style.a;
			return _class;
		case 'FontFamily':
			var name = style.a;
			return name;
		case 'FontSize':
			var i = style.a;
			return 'font-size-' + elm$core$String$fromInt(i);
		case 'Single':
			var _class = style.a;
			return _class;
		case 'Colored':
			var _class = style.a;
			return _class;
		case 'SpacingStyle':
			var cls = style.a;
			var x = style.b;
			var y = style.c;
			return cls;
		case 'PaddingStyle':
			var cls = style.a;
			var top = style.b;
			var right = style.c;
			var bottom = style.d;
			var left = style.e;
			return cls;
		case 'BorderWidth':
			var cls = style.a;
			var top = style.b;
			var right = style.c;
			var bottom = style.d;
			var left = style.e;
			return cls;
		case 'GridTemplateStyle':
			var template = style.a;
			return 'grid-rows-' + (A2(
				elm$core$String$join,
				'-',
				A2(elm$core$List$map, mdgriffith$elm_ui$Internal$Model$lengthClassName, template.rows)) + ('-cols-' + (A2(
				elm$core$String$join,
				'-',
				A2(elm$core$List$map, mdgriffith$elm_ui$Internal$Model$lengthClassName, template.columns)) + ('-space-x-' + (mdgriffith$elm_ui$Internal$Model$lengthClassName(template.spacing.a) + ('-space-y-' + mdgriffith$elm_ui$Internal$Model$lengthClassName(template.spacing.b)))))));
		case 'GridPosition':
			var pos = style.a;
			return 'gp grid-pos-' + (elm$core$String$fromInt(pos.row) + ('-' + (elm$core$String$fromInt(pos.col) + ('-' + (elm$core$String$fromInt(pos.width) + ('-' + elm$core$String$fromInt(pos.height)))))));
		case 'PseudoSelector':
			var selector = style.a;
			var subStyle = style.b;
			var name = function () {
				switch (selector.$) {
					case 'Focus':
						return 'fs';
					case 'Hover':
						return 'hv';
					default:
						return 'act';
				}
			}();
			return A2(
				elm$core$String$join,
				' ',
				A2(
					elm$core$List$map,
					function (sty) {
						var _n1 = mdgriffith$elm_ui$Internal$Model$getStyleName(sty);
						if (_n1 === '') {
							return '';
						} else {
							var styleName = _n1;
							return styleName + ('-' + name);
						}
					},
					subStyle));
		default:
			var x = style.a;
			return A2(
				elm$core$Maybe$withDefault,
				'',
				mdgriffith$elm_ui$Internal$Model$transformClass(x));
	}
};
var mdgriffith$elm_ui$Internal$Model$reduceStyles = F2(
	function (style, nevermind) {
		var cache = nevermind.a;
		var existing = nevermind.b;
		var styleName = mdgriffith$elm_ui$Internal$Model$getStyleName(style);
		return A2(elm$core$Set$member, styleName, cache) ? nevermind : _Utils_Tuple2(
			A2(elm$core$Set$insert, styleName, cache),
			A2(elm$core$List$cons, style, existing));
	});
var elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _n0 = f(mx);
		if (_n0.$ === 'Just') {
			var x = _n0.a;
			return A2(elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			elm$core$List$foldr,
			elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return elm$core$Maybe$Just(
				f(value));
		} else {
			return elm$core$Maybe$Nothing;
		}
	});
var elm$core$Tuple$mapFirst = F2(
	function (func, _n0) {
		var x = _n0.a;
		var y = _n0.b;
		return _Utils_Tuple2(
			func(x),
			y);
	});
var elm$core$Tuple$mapSecond = F2(
	function (func, _n0) {
		var x = _n0.a;
		var y = _n0.b;
		return _Utils_Tuple2(
			x,
			func(y));
	});
var mdgriffith$elm_ui$Internal$Model$Property = F2(
	function (a, b) {
		return {$: 'Property', a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Model$Style = F2(
	function (a, b) {
		return {$: 'Style', a: a, b: b};
	});
var elm$core$String$fromFloat = _String_fromNumber;
var mdgriffith$elm_ui$Internal$Model$formatColor = function (_n0) {
	var red = _n0.a;
	var green = _n0.b;
	var blue = _n0.c;
	var alpha = _n0.d;
	return 'rgba(' + (elm$core$String$fromInt(
		elm$core$Basics$round(red * 255)) + ((',' + elm$core$String$fromInt(
		elm$core$Basics$round(green * 255))) + ((',' + elm$core$String$fromInt(
		elm$core$Basics$round(blue * 255))) + (',' + (elm$core$String$fromFloat(alpha) + ')')))));
};
var mdgriffith$elm_ui$Internal$Model$formatBoxShadow = function (shadow) {
	return A2(
		elm$core$String$join,
		' ',
		A2(
			elm$core$List$filterMap,
			elm$core$Basics$identity,
			_List_fromArray(
				[
					shadow.inset ? elm$core$Maybe$Just('inset') : elm$core$Maybe$Nothing,
					elm$core$Maybe$Just(
					elm$core$String$fromFloat(shadow.offset.a) + 'px'),
					elm$core$Maybe$Just(
					elm$core$String$fromFloat(shadow.offset.b) + 'px'),
					elm$core$Maybe$Just(
					elm$core$String$fromFloat(shadow.blur) + 'px'),
					elm$core$Maybe$Just(
					elm$core$String$fromFloat(shadow.size) + 'px'),
					elm$core$Maybe$Just(
					mdgriffith$elm_ui$Internal$Model$formatColor(shadow.color))
				])));
};
var mdgriffith$elm_ui$Internal$Style$dot = function (c) {
	return '.' + c;
};
var mdgriffith$elm_ui$Internal$Model$renderFocusStyle = function (focus) {
	return A2(
		mdgriffith$elm_ui$Internal$Model$Style,
		mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.any) + (':focus .focusable, ' + (mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.any) + '.focusable:focus')),
		A2(
			elm$core$List$filterMap,
			elm$core$Basics$identity,
			_List_fromArray(
				[
					A2(
					elm$core$Maybe$map,
					function (color) {
						return A2(
							mdgriffith$elm_ui$Internal$Model$Property,
							'border-color',
							mdgriffith$elm_ui$Internal$Model$formatColor(color));
					},
					focus.borderColor),
					A2(
					elm$core$Maybe$map,
					function (color) {
						return A2(
							mdgriffith$elm_ui$Internal$Model$Property,
							'background-color',
							mdgriffith$elm_ui$Internal$Model$formatColor(color));
					},
					focus.backgroundColor),
					A2(
					elm$core$Maybe$map,
					function (shadow) {
						return A2(
							mdgriffith$elm_ui$Internal$Model$Property,
							'box-shadow',
							mdgriffith$elm_ui$Internal$Model$formatBoxShadow(
								{
									blur: shadow.blur,
									color: shadow.color,
									inset: false,
									offset: A2(
										elm$core$Tuple$mapSecond,
										elm$core$Basics$toFloat,
										A2(elm$core$Tuple$mapFirst, elm$core$Basics$toFloat, shadow.offset)),
									size: shadow.size
								}));
					},
					focus.shadow),
					elm$core$Maybe$Just(
					A2(mdgriffith$elm_ui$Internal$Model$Property, 'outline', 'none'))
				])));
};
var elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var mdgriffith$elm_ui$Internal$Style$Batch = function (a) {
	return {$: 'Batch', a: a};
};
var mdgriffith$elm_ui$Internal$Style$Child = F2(
	function (a, b) {
		return {$: 'Child', a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Style$Class = F2(
	function (a, b) {
		return {$: 'Class', a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Style$Descriptor = F2(
	function (a, b) {
		return {$: 'Descriptor', a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Style$Left = {$: 'Left'};
var mdgriffith$elm_ui$Internal$Style$Prop = F2(
	function (a, b) {
		return {$: 'Prop', a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Style$Right = {$: 'Right'};
var mdgriffith$elm_ui$Internal$Style$Self = function (a) {
	return {$: 'Self', a: a};
};
var mdgriffith$elm_ui$Internal$Style$Supports = F2(
	function (a, b) {
		return {$: 'Supports', a: a, b: b};
	});
var elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3(elm$core$List$foldr, elm$core$List$cons, ys, xs);
		}
	});
var elm$core$List$concat = function (lists) {
	return A3(elm$core$List$foldr, elm$core$List$append, _List_Nil, lists);
};
var elm$core$List$concatMap = F2(
	function (f, list) {
		return elm$core$List$concat(
			A2(elm$core$List$map, f, list));
	});
var mdgriffith$elm_ui$Internal$Style$Content = function (a) {
	return {$: 'Content', a: a};
};
var mdgriffith$elm_ui$Internal$Style$Bottom = {$: 'Bottom'};
var mdgriffith$elm_ui$Internal$Style$CenterX = {$: 'CenterX'};
var mdgriffith$elm_ui$Internal$Style$CenterY = {$: 'CenterY'};
var mdgriffith$elm_ui$Internal$Style$Top = {$: 'Top'};
var mdgriffith$elm_ui$Internal$Style$alignments = _List_fromArray(
	[mdgriffith$elm_ui$Internal$Style$Top, mdgriffith$elm_ui$Internal$Style$Bottom, mdgriffith$elm_ui$Internal$Style$Right, mdgriffith$elm_ui$Internal$Style$Left, mdgriffith$elm_ui$Internal$Style$CenterX, mdgriffith$elm_ui$Internal$Style$CenterY]);
var mdgriffith$elm_ui$Internal$Style$contentName = function (desc) {
	switch (desc.a.$) {
		case 'Top':
			var _n1 = desc.a;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.contentTop);
		case 'Bottom':
			var _n2 = desc.a;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.contentBottom);
		case 'Right':
			var _n3 = desc.a;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.contentRight);
		case 'Left':
			var _n4 = desc.a;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.contentLeft);
		case 'CenterX':
			var _n5 = desc.a;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.contentCenterX);
		default:
			var _n6 = desc.a;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.contentCenterY);
	}
};
var mdgriffith$elm_ui$Internal$Style$selfName = function (desc) {
	switch (desc.a.$) {
		case 'Top':
			var _n1 = desc.a;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.alignTop);
		case 'Bottom':
			var _n2 = desc.a;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.alignBottom);
		case 'Right':
			var _n3 = desc.a;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.alignRight);
		case 'Left':
			var _n4 = desc.a;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.alignLeft);
		case 'CenterX':
			var _n5 = desc.a;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.alignCenterX);
		default:
			var _n6 = desc.a;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.alignCenterY);
	}
};
var mdgriffith$elm_ui$Internal$Style$describeAlignment = function (values) {
	var createDescription = function (alignment) {
		var _n0 = values(alignment);
		var content = _n0.a;
		var indiv = _n0.b;
		return _List_fromArray(
			[
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$contentName(
					mdgriffith$elm_ui$Internal$Style$Content(alignment)),
				content),
				A2(
				mdgriffith$elm_ui$Internal$Style$Child,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.any),
				_List_fromArray(
					[
						A2(
						mdgriffith$elm_ui$Internal$Style$Descriptor,
						mdgriffith$elm_ui$Internal$Style$selfName(
							mdgriffith$elm_ui$Internal$Style$Self(alignment)),
						indiv)
					]))
			]);
	};
	return mdgriffith$elm_ui$Internal$Style$Batch(
		A2(elm$core$List$concatMap, createDescription, mdgriffith$elm_ui$Internal$Style$alignments));
};
var mdgriffith$elm_ui$Internal$Style$elDescription = _List_fromArray(
	[
		A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
		A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'column'),
		A2(mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre'),
		A2(
		mdgriffith$elm_ui$Internal$Style$Descriptor,
		mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.hasBehind),
		_List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
				A2(
				mdgriffith$elm_ui$Internal$Style$Child,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.behind),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '-1')
					]))
			])),
		A2(
		mdgriffith$elm_ui$Internal$Style$Descriptor,
		mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.seButton),
		_List_fromArray(
			[
				A2(
				mdgriffith$elm_ui$Internal$Style$Child,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.text),
				_List_fromArray(
					[
						A2(
						mdgriffith$elm_ui$Internal$Style$Descriptor,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.heightFill),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Descriptor,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.widthFill),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'auto !important')
							]))
					]))
			])),
		A2(
		mdgriffith$elm_ui$Internal$Style$Child,
		mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.heightContent),
		_List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto')
			])),
		A2(
		mdgriffith$elm_ui$Internal$Style$Child,
		mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.heightFill),
		_List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '100000')
			])),
		A2(
		mdgriffith$elm_ui$Internal$Style$Child,
		mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.widthFill),
		_List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
			])),
		A2(
		mdgriffith$elm_ui$Internal$Style$Child,
		mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.widthContent),
		_List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
			])),
		mdgriffith$elm_ui$Internal$Style$describeAlignment(
		function (alignment) {
			switch (alignment.$) {
				case 'Top':
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
							]),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important'),
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', '0 !important')
							]));
				case 'Bottom':
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
							]),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', '0 !important')
							]));
				case 'Right':
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
							]),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-end')
							]));
				case 'Left':
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
							]),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
							]));
				case 'CenterX':
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
							]),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'center')
							]));
				default:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2(
								mdgriffith$elm_ui$Internal$Style$Child,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.any),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto'),
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto')
									]))
							]),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important')
							]));
			}
		})
	]);
var mdgriffith$elm_ui$Internal$Style$gridAlignments = function (values) {
	var createDescription = function (alignment) {
		return _List_fromArray(
			[
				A2(
				mdgriffith$elm_ui$Internal$Style$Child,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.any),
				_List_fromArray(
					[
						A2(
						mdgriffith$elm_ui$Internal$Style$Descriptor,
						mdgriffith$elm_ui$Internal$Style$selfName(
							mdgriffith$elm_ui$Internal$Style$Self(alignment)),
						values(alignment))
					]))
			]);
	};
	return mdgriffith$elm_ui$Internal$Style$Batch(
		A2(elm$core$List$concatMap, createDescription, mdgriffith$elm_ui$Internal$Style$alignments));
};
var mdgriffith$elm_ui$Internal$Style$Above = {$: 'Above'};
var mdgriffith$elm_ui$Internal$Style$Behind = {$: 'Behind'};
var mdgriffith$elm_ui$Internal$Style$Below = {$: 'Below'};
var mdgriffith$elm_ui$Internal$Style$OnLeft = {$: 'OnLeft'};
var mdgriffith$elm_ui$Internal$Style$OnRight = {$: 'OnRight'};
var mdgriffith$elm_ui$Internal$Style$Within = {$: 'Within'};
var mdgriffith$elm_ui$Internal$Style$locations = function () {
	var loc = mdgriffith$elm_ui$Internal$Style$Above;
	var _n0 = function () {
		switch (loc.$) {
			case 'Above':
				return _Utils_Tuple0;
			case 'Below':
				return _Utils_Tuple0;
			case 'OnRight':
				return _Utils_Tuple0;
			case 'OnLeft':
				return _Utils_Tuple0;
			case 'Within':
				return _Utils_Tuple0;
			default:
				return _Utils_Tuple0;
		}
	}();
	return _List_fromArray(
		[mdgriffith$elm_ui$Internal$Style$Above, mdgriffith$elm_ui$Internal$Style$Below, mdgriffith$elm_ui$Internal$Style$OnRight, mdgriffith$elm_ui$Internal$Style$OnLeft, mdgriffith$elm_ui$Internal$Style$Within, mdgriffith$elm_ui$Internal$Style$Behind]);
}();
var mdgriffith$elm_ui$Internal$Style$baseSheet = _List_fromArray(
	[
		A2(
		mdgriffith$elm_ui$Internal$Style$Class,
		'html,body',
		_List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'padding', '0'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0')
			])),
		A2(
		mdgriffith$elm_ui$Internal$Style$Class,
		_Utils_ap(
			mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.any),
			_Utils_ap(
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.single),
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.imageContainer))),
		_List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'block')
			])),
		A2(
		mdgriffith$elm_ui$Internal$Style$Class,
		mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.any) + ':focus',
		_List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'outline', 'none')
			])),
		A2(
		mdgriffith$elm_ui$Internal$Style$Class,
		mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.root),
		_List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'min-height', '100%'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				_Utils_ap(
					mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.any),
					mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.heightFill)),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.heightFill),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%')
							]))
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Child,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.inFront),
				_List_fromArray(
					[
						A2(
						mdgriffith$elm_ui$Internal$Style$Descriptor,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.nearby),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'fixed')
							]))
					]))
			])),
		A2(
		mdgriffith$elm_ui$Internal$Style$Class,
		mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.nearby),
		_List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'relative'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'border', 'none'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'row'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.single),
				mdgriffith$elm_ui$Internal$Style$elDescription),
				mdgriffith$elm_ui$Internal$Style$Batch(
				function (fn) {
					return A2(elm$core$List$map, fn, mdgriffith$elm_ui$Internal$Style$locations);
				}(
					function (loc) {
						switch (loc.$) {
							case 'Above':
								return A2(
									mdgriffith$elm_ui$Internal$Style$Descriptor,
									mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.above),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'bottom', '100%'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2(
											mdgriffith$elm_ui$Internal$Style$Child,
											mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.heightFill),
											_List_fromArray(
												[
													A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto')
												])),
											A2(
											mdgriffith$elm_ui$Internal$Style$Child,
											mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.widthFill),
											_List_fromArray(
												[
													A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
												])),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							case 'Below':
								return A2(
									mdgriffith$elm_ui$Internal$Style$Descriptor,
									mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.below),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'bottom', '0'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', '0'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												])),
											A2(
											mdgriffith$elm_ui$Internal$Style$Child,
											mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.heightFill),
											_List_fromArray(
												[
													A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto')
												]))
										]));
							case 'OnRight':
								return A2(
									mdgriffith$elm_ui$Internal$Style$Descriptor,
									mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.onRight),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'left', '100%'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							case 'OnLeft':
								return A2(
									mdgriffith$elm_ui$Internal$Style$Descriptor,
									mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.onLeft),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'right', '100%'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							case 'Within':
								return A2(
									mdgriffith$elm_ui$Internal$Style$Descriptor,
									mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.inFront),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							default:
								return A2(
									mdgriffith$elm_ui$Internal$Style$Descriptor,
									mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.behind),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
						}
					}))
			])),
		A2(
		mdgriffith$elm_ui$Internal$Style$Class,
		mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.any),
		_List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'relative'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'border', 'none'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '0'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'row'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'resize', 'none'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-feature-settings', 'inherit'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'box-sizing', 'border-box'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'padding', '0'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'border-width', '0'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'solid'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-size', 'inherit'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'color', 'inherit'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-family', 'inherit'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'line-height', '1'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', 'inherit'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'none'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-style', 'inherit'),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.wrapped),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-wrap', 'wrap')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.noTextSelection),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, '-moz-user-select', 'none'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, '-webkit-user-select', 'none'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, '-ms-user-select', 'none'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'user-select', 'none')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.cursorPointer),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'cursor', 'pointer')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.cursorText),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'cursor', 'text')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.passPointerEvents),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none !important')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.capturePointerEvents),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto !important')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.transparent),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.opaque),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap(mdgriffith$elm_ui$Internal$Style$classes.hover, mdgriffith$elm_ui$Internal$Style$classes.transparent)) + ':hover',
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap(mdgriffith$elm_ui$Internal$Style$classes.hover, mdgriffith$elm_ui$Internal$Style$classes.opaque)) + ':hover',
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap(mdgriffith$elm_ui$Internal$Style$classes.focus, mdgriffith$elm_ui$Internal$Style$classes.transparent)) + ':focus',
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap(mdgriffith$elm_ui$Internal$Style$classes.focus, mdgriffith$elm_ui$Internal$Style$classes.opaque)) + ':focus',
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap(mdgriffith$elm_ui$Internal$Style$classes.active, mdgriffith$elm_ui$Internal$Style$classes.transparent)) + ':active',
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap(mdgriffith$elm_ui$Internal$Style$classes.active, mdgriffith$elm_ui$Internal$Style$classes.opaque)) + ':active',
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.transition),
				_List_fromArray(
					[
						A2(
						mdgriffith$elm_ui$Internal$Style$Prop,
						'transition',
						A2(
							elm$core$String$join,
							', ',
							A2(
								elm$core$List$map,
								function (x) {
									return x + ' 160ms';
								},
								_List_fromArray(
									['transform', 'opacity', 'filter', 'background-color', 'color', 'font-size']))))
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.scrollbars),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'overflow', 'auto'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.scrollbarsX),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-x', 'auto'),
						A2(
						mdgriffith$elm_ui$Internal$Style$Descriptor,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.row),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
							]))
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.scrollbarsY),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-y', 'auto'),
						A2(
						mdgriffith$elm_ui$Internal$Style$Descriptor,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.column),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Descriptor,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.single),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
							]))
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.clip),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'overflow', 'hidden')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.clipX),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-x', 'hidden')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.clipY),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-y', 'hidden')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.widthContent),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', 'auto')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.borderNone),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'border-width', '0')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.borderDashed),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'dashed')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.borderDotted),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'dotted')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.borderSolid),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'solid')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.text),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-block')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.inputText),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'line-height', '1.05')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.single),
				mdgriffith$elm_ui$Internal$Style$elDescription),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.row),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'row'),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.any),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', '0%'),
								A2(
								mdgriffith$elm_ui$Internal$Style$Descriptor,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.widthExact),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto')
									]))
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.heightFill),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch !important')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.heightFillPortion),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch !important')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.widthFill),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '100000')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.container),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0'),
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + mdgriffith$elm_ui$Internal$Style$classes.alignContainerRight,
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						's:first-of-type.' + mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterX,
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								mdgriffith$elm_ui$Internal$Style$Child,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.alignCenterX),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-left', 'auto !important')
									]))
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterX,
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								mdgriffith$elm_ui$Internal$Style$Child,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.alignCenterX),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-right', 'auto !important')
									]))
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						's:only-of-type.' + mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterX,
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								mdgriffith$elm_ui$Internal$Style$Child,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.alignCenterY),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important')
									]))
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + (mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterX + ' ~ u'),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + (mdgriffith$elm_ui$Internal$Style$classes.alignContainerRight + (' ~ s.' + mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterX)),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment.$) {
								case 'Top':
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
											]),
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
											]));
								case 'Bottom':
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
											]),
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-end')
											]));
								case 'Right':
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
											]),
										_List_Nil);
								case 'Left':
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
											]),
										_List_Nil);
								case 'CenterX':
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'center')
											]),
										_List_Nil);
								default:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
											]),
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'center')
											]));
							}
						}),
						A2(
						mdgriffith$elm_ui$Internal$Style$Descriptor,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.spaceEvenly),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'space-between')
							]))
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.column),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'column'),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.heightFill),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '100000')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.widthFill),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.widthFillPortion),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.widthContent),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + mdgriffith$elm_ui$Internal$Style$classes.alignContainerBottom,
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						's:first-of-type.' + mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterY,
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								mdgriffith$elm_ui$Internal$Style$Child,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.alignCenterY),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', '0 !important')
									]))
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterY,
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								mdgriffith$elm_ui$Internal$Style$Child,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.alignCenterY),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important'),
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', '0 !important')
									]))
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						's:only-of-type.' + mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterY,
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								mdgriffith$elm_ui$Internal$Style$Child,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.alignCenterY),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important')
									]))
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + (mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterY + ' ~ u'),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + (mdgriffith$elm_ui$Internal$Style$classes.alignContainerBottom + (' ~ s.' + mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterY)),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment.$) {
								case 'Top':
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
											]),
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto')
											]));
								case 'Bottom':
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
											]),
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto')
											]));
								case 'Right':
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
											]),
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-end')
											]));
								case 'Left':
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
											]),
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
											]));
								case 'CenterX':
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
											]),
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'center')
											]));
								default:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'center')
											]),
										_List_Nil);
							}
						}),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.container),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0'),
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch !important')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Descriptor,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.spaceEvenly),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'space-between')
							]))
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.grid),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', '-ms-grid'),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						'.gp',
						_List_fromArray(
							[
								A2(
								mdgriffith$elm_ui$Internal$Style$Child,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.any),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
									]))
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Supports,
						_Utils_Tuple2('display', 'grid'),
						_List_fromArray(
							[
								_Utils_Tuple2('display', 'grid')
							])),
						mdgriffith$elm_ui$Internal$Style$gridAlignments(
						function (alignment) {
							switch (alignment.$) {
								case 'Top':
									return _List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
										]);
								case 'Bottom':
									return _List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
										]);
								case 'Right':
									return _List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
										]);
								case 'Left':
									return _List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
										]);
								case 'CenterX':
									return _List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
										]);
								default:
									return _List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'center')
										]);
							}
						})
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.page),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'block'),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.any + ':first-child'),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(
							mdgriffith$elm_ui$Internal$Style$classes.any + (mdgriffith$elm_ui$Internal$Style$selfName(
								mdgriffith$elm_ui$Internal$Style$Self(mdgriffith$elm_ui$Internal$Style$Left)) + (':first-child + .' + mdgriffith$elm_ui$Internal$Style$classes.any))),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(
							mdgriffith$elm_ui$Internal$Style$classes.any + (mdgriffith$elm_ui$Internal$Style$selfName(
								mdgriffith$elm_ui$Internal$Style$Self(mdgriffith$elm_ui$Internal$Style$Right)) + (':first-child + .' + mdgriffith$elm_ui$Internal$Style$classes.any))),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important')
							])),
						mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment.$) {
								case 'Top':
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 'Bottom':
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 'Right':
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'right'),
												A2(
												mdgriffith$elm_ui$Internal$Style$Descriptor,
												'::after',
												_List_fromArray(
													[
														A2(mdgriffith$elm_ui$Internal$Style$Prop, 'content', '\"\"'),
														A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'table'),
														A2(mdgriffith$elm_ui$Internal$Style$Prop, 'clear', 'both')
													]))
											]));
								case 'Left':
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'left'),
												A2(
												mdgriffith$elm_ui$Internal$Style$Descriptor,
												'::after',
												_List_fromArray(
													[
														A2(mdgriffith$elm_ui$Internal$Style$Prop, 'content', '\"\"'),
														A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'table'),
														A2(mdgriffith$elm_ui$Internal$Style$Prop, 'clear', 'both')
													]))
											]));
								case 'CenterX':
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								default:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
							}
						})
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.inputMultiline),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre-wrap')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.paragraph),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'block'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal'),
						A2(
						mdgriffith$elm_ui$Internal$Style$Descriptor,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.hasBehind),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
								A2(
								mdgriffith$elm_ui$Internal$Style$Child,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.behind),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '-1')
									]))
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.text),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.single),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal'),
								A2(
								mdgriffith$elm_ui$Internal$Style$Descriptor,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.inFront),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								mdgriffith$elm_ui$Internal$Style$Descriptor,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.behind),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								mdgriffith$elm_ui$Internal$Style$Descriptor,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.above),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								mdgriffith$elm_ui$Internal$Style$Descriptor,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.below),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								mdgriffith$elm_ui$Internal$Style$Descriptor,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.onRight),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								mdgriffith$elm_ui$Internal$Style$Descriptor,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.onLeft),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								mdgriffith$elm_ui$Internal$Style$Child,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.text),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal')
									]))
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.row),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-flex')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.column),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-flex')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.grid),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-grid')
							])),
						mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment.$) {
								case 'Top':
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 'Bottom':
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 'Right':
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'right')
											]));
								case 'Left':
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'left')
											]));
								case 'CenterX':
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								default:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
							}
						})
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				'.hidden',
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'none')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.textThin),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '100')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.textExtraLight),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '200')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.textLight),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '300')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.textNormalWeight),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '400')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.textMedium),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '500')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.textSemiBold),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '600')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.bold),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '700')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.textExtraBold),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '800')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.textHeavy),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '900')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.italic),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-style', 'italic')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.strike),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'line-through')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.underline),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'underline'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip-ink', 'auto'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip', 'ink')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				_Utils_ap(
					mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.underline),
					mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.strike)),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'line-through underline'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip-ink', 'auto'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip', 'ink')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.textUnitalicized),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-style', 'normal')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.textJustify),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'justify')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.textJustifyAll),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'justify-all')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.textCenter),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'center')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.textRight),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'right')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.textLeft),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'left')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				'.modal',
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'fixed'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none')
					]))
			]))
	]);
var mdgriffith$elm_ui$Internal$Style$fontVariant = function (_var) {
	return _List_fromArray(
		[
			A2(
			mdgriffith$elm_ui$Internal$Style$Class,
			'.v-' + _var,
			_List_fromArray(
				[
					A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-feature-settings', '\"' + (_var + '\"'))
				])),
			A2(
			mdgriffith$elm_ui$Internal$Style$Class,
			'.v-' + (_var + '-off'),
			_List_fromArray(
				[
					A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-feature-settings', '\"' + (_var + '\" 0'))
				]))
		]);
};
var mdgriffith$elm_ui$Internal$Style$commonValues = elm$core$List$concat(
	_List_fromArray(
		[
			A2(
			elm$core$List$map,
			function (x) {
				return A2(
					mdgriffith$elm_ui$Internal$Style$Class,
					'.border-' + elm$core$String$fromInt(x),
					_List_fromArray(
						[
							A2(
							mdgriffith$elm_ui$Internal$Style$Prop,
							'border-width',
							elm$core$String$fromInt(x) + 'px')
						]));
			},
			A2(elm$core$List$range, 0, 6)),
			A2(
			elm$core$List$map,
			function (i) {
				return A2(
					mdgriffith$elm_ui$Internal$Style$Class,
					'.font-size-' + elm$core$String$fromInt(i),
					_List_fromArray(
						[
							A2(
							mdgriffith$elm_ui$Internal$Style$Prop,
							'font-size',
							elm$core$String$fromInt(i) + 'px')
						]));
			},
			A2(elm$core$List$range, 8, 32)),
			A2(
			elm$core$List$map,
			function (i) {
				return A2(
					mdgriffith$elm_ui$Internal$Style$Class,
					'.p-' + elm$core$String$fromInt(i),
					_List_fromArray(
						[
							A2(
							mdgriffith$elm_ui$Internal$Style$Prop,
							'padding',
							elm$core$String$fromInt(i) + 'px')
						]));
			},
			A2(elm$core$List$range, 0, 24)),
			_List_fromArray(
			[
				A2(
				mdgriffith$elm_ui$Internal$Style$Class,
				'.v-smcp',
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-variant', 'small-caps')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Class,
				'.v-smcp-off',
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-variant', 'normal')
					]))
			]),
			mdgriffith$elm_ui$Internal$Style$fontVariant('zero'),
			mdgriffith$elm_ui$Internal$Style$fontVariant('onum'),
			mdgriffith$elm_ui$Internal$Style$fontVariant('liga'),
			mdgriffith$elm_ui$Internal$Style$fontVariant('dlig'),
			mdgriffith$elm_ui$Internal$Style$fontVariant('ordn'),
			mdgriffith$elm_ui$Internal$Style$fontVariant('tnum'),
			mdgriffith$elm_ui$Internal$Style$fontVariant('afrc'),
			mdgriffith$elm_ui$Internal$Style$fontVariant('frac')
		]));
var mdgriffith$elm_ui$Internal$Style$explainer = '\n.explain {\n    border: 6px solid rgb(174, 121, 15) !important;\n}\n.explain > .' + (mdgriffith$elm_ui$Internal$Style$classes.any + (' {\n    border: 4px dashed rgb(0, 151, 167) !important;\n}\n\n.ctr {\n    border: none !important;\n}\n.explain > .ctr > .' + (mdgriffith$elm_ui$Internal$Style$classes.any + ' {\n    border: 4px dashed rgb(0, 151, 167) !important;\n}\n\n')));
var mdgriffith$elm_ui$Internal$Style$sliderOverrides = '\n\n/* General Input Reset */\ninput[type=range] {\n  -webkit-appearance: none; /* Hides the slider so that custom slider can be made */\n  /* width: 100%;  Specific width is required for Firefox. */\n  background: transparent; /* Otherwise white in Chrome */\n  position:absolute;\n  left:0;\n  top:0;\n  z-index:10;\n  width: 100%;\n  outline: dashed 1px;\n  height: 100%;\n  opacity: 0;\n}\n\n/* Hide all syling for track */\ninput[type=range]::-moz-range-track {\n    background: transparent;\n    cursor: pointer;\n}\ninput[type=range]::-ms-track {\n    background: transparent;\n    cursor: pointer;\n}\ninput[type=range]::-webkit-slider-runnable-track {\n    background: transparent;\n    cursor: pointer;\n}\n\n/* Thumbs */\ninput[type=range]::-webkit-slider-thumb {\n    -webkit-appearance: none;\n    opacity: 0.5;\n    width: 80px;\n    height: 80px;\n    background-color: black;\n    border:none;\n    border-radius: 5px;\n}\ninput[type=range]::-moz-range-thumb {\n    opacity: 0.5;\n    width: 80px;\n    height: 80px;\n    background-color: black;\n    border:none;\n    border-radius: 5px;\n}\ninput[type=range]::-ms-thumb {\n    opacity: 0.5;\n    width: 80px;\n    height: 80px;\n    background-color: black;\n    border:none;\n    border-radius: 5px;\n}\ninput[type=range][orient=vertical]{\n    writing-mode: bt-lr; /* IE */\n    -webkit-appearance: slider-vertical;  /* WebKit */\n}\n';
var mdgriffith$elm_ui$Internal$Style$overrides = '@media screen and (-ms-high-contrast: active), (-ms-high-contrast: none) {' + (mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.any) + (mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.row) + (' > ' + (mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.any) + (' { flex-basis: auto !important; } ' + (mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.any) + (mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.row) + (' > ' + (mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.any) + (mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.container) + (' { flex-basis: auto !important; }}' + (mdgriffith$elm_ui$Internal$Style$sliderOverrides + mdgriffith$elm_ui$Internal$Style$explainer))))))))))));
var elm$core$String$concat = function (strings) {
	return A2(elm$core$String$join, '', strings);
};
var mdgriffith$elm_ui$Internal$Style$Intermediate = function (a) {
	return {$: 'Intermediate', a: a};
};
var mdgriffith$elm_ui$Internal$Style$emptyIntermediate = F2(
	function (selector, closing) {
		return mdgriffith$elm_ui$Internal$Style$Intermediate(
			{closing: closing, others: _List_Nil, props: _List_Nil, selector: selector});
	});
var mdgriffith$elm_ui$Internal$Style$renderRules = F2(
	function (_n0, rulesToRender) {
		var parent = _n0.a;
		var generateIntermediates = F2(
			function (rule, rendered) {
				switch (rule.$) {
					case 'Prop':
						var name = rule.a;
						var val = rule.b;
						return _Utils_update(
							rendered,
							{
								props: A2(
									elm$core$List$cons,
									_Utils_Tuple2(name, val),
									rendered.props)
							});
					case 'Supports':
						var _n2 = rule.a;
						var prop = _n2.a;
						var value = _n2.b;
						var props = rule.b;
						return _Utils_update(
							rendered,
							{
								others: A2(
									elm$core$List$cons,
									mdgriffith$elm_ui$Internal$Style$Intermediate(
										{closing: '\n}', others: _List_Nil, props: props, selector: '@supports (' + (prop + (':' + (value + (') {' + parent.selector))))}),
									rendered.others)
							});
					case 'Adjacent':
						var selector = rule.a;
						var adjRules = rule.b;
						return _Utils_update(
							rendered,
							{
								others: A2(
									elm$core$List$cons,
									A2(
										mdgriffith$elm_ui$Internal$Style$renderRules,
										A2(mdgriffith$elm_ui$Internal$Style$emptyIntermediate, parent.selector + (' + ' + selector), ''),
										adjRules),
									rendered.others)
							});
					case 'Child':
						var child = rule.a;
						var childRules = rule.b;
						return _Utils_update(
							rendered,
							{
								others: A2(
									elm$core$List$cons,
									A2(
										mdgriffith$elm_ui$Internal$Style$renderRules,
										A2(mdgriffith$elm_ui$Internal$Style$emptyIntermediate, parent.selector + (' > ' + child), ''),
										childRules),
									rendered.others)
							});
					case 'Descriptor':
						var descriptor = rule.a;
						var descriptorRules = rule.b;
						return _Utils_update(
							rendered,
							{
								others: A2(
									elm$core$List$cons,
									A2(
										mdgriffith$elm_ui$Internal$Style$renderRules,
										A2(
											mdgriffith$elm_ui$Internal$Style$emptyIntermediate,
											_Utils_ap(parent.selector, descriptor),
											''),
										descriptorRules),
									rendered.others)
							});
					default:
						var batched = rule.a;
						return _Utils_update(
							rendered,
							{
								others: A2(
									elm$core$List$cons,
									A2(
										mdgriffith$elm_ui$Internal$Style$renderRules,
										A2(mdgriffith$elm_ui$Internal$Style$emptyIntermediate, parent.selector, ''),
										batched),
									rendered.others)
							});
				}
			});
		return mdgriffith$elm_ui$Internal$Style$Intermediate(
			A3(elm$core$List$foldr, generateIntermediates, parent, rulesToRender));
	});
var mdgriffith$elm_ui$Internal$Style$renderCompact = function (styleClasses) {
	var renderValues = function (values) {
		return elm$core$String$concat(
			A2(
				elm$core$List$map,
				function (_n3) {
					var x = _n3.a;
					var y = _n3.b;
					return x + (':' + (y + ';'));
				},
				values));
	};
	var renderClass = function (rule) {
		var _n2 = rule.props;
		if (!_n2.b) {
			return '';
		} else {
			return rule.selector + ('{' + (renderValues(rule.props) + (rule.closing + '}')));
		}
	};
	var renderIntermediate = function (_n0) {
		var rule = _n0.a;
		return _Utils_ap(
			renderClass(rule),
			elm$core$String$concat(
				A2(elm$core$List$map, renderIntermediate, rule.others)));
	};
	return elm$core$String$concat(
		A2(
			elm$core$List$map,
			renderIntermediate,
			A3(
				elm$core$List$foldr,
				F2(
					function (_n1, existing) {
						var name = _n1.a;
						var styleRules = _n1.b;
						return A2(
							elm$core$List$cons,
							A2(
								mdgriffith$elm_ui$Internal$Style$renderRules,
								A2(mdgriffith$elm_ui$Internal$Style$emptyIntermediate, name, ''),
								styleRules),
							existing);
					}),
				_List_Nil,
				styleClasses)));
};
var mdgriffith$elm_ui$Internal$Style$rules = _Utils_ap(
	mdgriffith$elm_ui$Internal$Style$overrides,
	mdgriffith$elm_ui$Internal$Style$renderCompact(
		_Utils_ap(mdgriffith$elm_ui$Internal$Style$baseSheet, mdgriffith$elm_ui$Internal$Style$commonValues)));
var mdgriffith$elm_ui$Internal$Model$staticRoot = A3(
	elm$virtual_dom$VirtualDom$node,
	'style',
	_List_Nil,
	_List_fromArray(
		[
			elm$virtual_dom$VirtualDom$text(mdgriffith$elm_ui$Internal$Style$rules)
		]));
var elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var elm$core$Basics$negate = function (n) {
	return -n;
};
var elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var mdgriffith$elm_ui$Internal$Model$fontName = function (font) {
	switch (font.$) {
		case 'Serif':
			return 'serif';
		case 'SansSerif':
			return 'sans-serif';
		case 'Monospace':
			return 'monospace';
		case 'Typeface':
			var name = font.a;
			return '\"' + (name + '\"');
		case 'ImportFont':
			var name = font.a;
			var url = font.b;
			return '\"' + (name + '\"');
		default:
			var name = font.a.name;
			return '\"' + (name + '\"');
	}
};
var mdgriffith$elm_ui$Internal$Model$isSmallCaps = function (_var) {
	switch (_var.$) {
		case 'VariantActive':
			var name = _var.a;
			return name === 'smcp';
		case 'VariantOff':
			var name = _var.a;
			return false;
		default:
			var name = _var.a;
			var index = _var.b;
			return (name === 'smcp') && (index === 1);
	}
};
var mdgriffith$elm_ui$Internal$Model$hasSmallCaps = function (typeface) {
	if (typeface.$ === 'FontWith') {
		var font = typeface.a;
		return A2(elm$core$List$any, mdgriffith$elm_ui$Internal$Model$isSmallCaps, font.variants);
	} else {
		return false;
	}
};
var mdgriffith$elm_ui$Internal$Model$renderProps = F3(
	function (force, _n0, existing) {
		var key = _n0.a;
		var val = _n0.b;
		return force ? (existing + ('\n  ' + (key + (': ' + (val + ' !important;'))))) : (existing + ('\n  ' + (key + (': ' + (val + ';')))));
	});
var mdgriffith$elm_ui$Internal$Model$bracket = F2(
	function (selector, rules) {
		var renderPair = function (_n0) {
			var name = _n0.a;
			var val = _n0.b;
			return name + (': ' + (val + ';'));
		};
		return selector + (' {' + (A2(
			elm$core$String$join,
			'',
			A2(elm$core$List$map, renderPair, rules)) + '}'));
	});
var mdgriffith$elm_ui$Internal$Model$fontRule = F3(
	function (name, modifier, _n0) {
		var parentAdj = _n0.a;
		var textAdjustment = _n0.b;
		return _List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Model$bracket, '.' + (name + ('.' + (modifier + (', ' + ('.' + (name + (' .' + modifier))))))), parentAdj),
				A2(mdgriffith$elm_ui$Internal$Model$bracket, '.' + (name + ('.' + (modifier + ('> .' + (mdgriffith$elm_ui$Internal$Style$classes.text + (', .' + (name + (' .' + (modifier + (' > .' + mdgriffith$elm_ui$Internal$Style$classes.text)))))))))), textAdjustment)
			]);
	});
var mdgriffith$elm_ui$Internal$Model$renderFontAdjustmentRule = F3(
	function (fontToAdjust, _n0, otherFontName) {
		var full = _n0.a;
		var capital = _n0.b;
		var name = _Utils_eq(fontToAdjust, otherFontName) ? fontToAdjust : (otherFontName + (' .' + fontToAdjust));
		return A2(
			elm$core$String$join,
			' ',
			_Utils_ap(
				A3(mdgriffith$elm_ui$Internal$Model$fontRule, name, mdgriffith$elm_ui$Internal$Style$classes.sizeByCapital, capital),
				A3(mdgriffith$elm_ui$Internal$Model$fontRule, name, mdgriffith$elm_ui$Internal$Style$classes.fullSize, full)));
	});
var mdgriffith$elm_ui$Internal$Model$renderNullAdjustmentRule = F2(
	function (fontToAdjust, otherFontName) {
		var name = _Utils_eq(fontToAdjust, otherFontName) ? fontToAdjust : (otherFontName + (' .' + fontToAdjust));
		return A2(
			elm$core$String$join,
			' ',
			_List_fromArray(
				[
					A2(
					mdgriffith$elm_ui$Internal$Model$bracket,
					'.' + (name + ('.' + (mdgriffith$elm_ui$Internal$Style$classes.sizeByCapital + (', ' + ('.' + (name + (' .' + mdgriffith$elm_ui$Internal$Style$classes.sizeByCapital))))))),
					_List_fromArray(
						[
							_Utils_Tuple2('line-height', '1')
						])),
					A2(
					mdgriffith$elm_ui$Internal$Model$bracket,
					'.' + (name + ('.' + (mdgriffith$elm_ui$Internal$Style$classes.sizeByCapital + ('> .' + (mdgriffith$elm_ui$Internal$Style$classes.text + (', .' + (name + (' .' + (mdgriffith$elm_ui$Internal$Style$classes.sizeByCapital + (' > .' + mdgriffith$elm_ui$Internal$Style$classes.text)))))))))),
					_List_fromArray(
						[
							_Utils_Tuple2('vertical-align', '0'),
							_Utils_Tuple2('line-height', '1')
						]))
				]));
	});
var elm$core$Basics$neq = _Utils_notEqual;
var elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2(elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var elm$core$List$maximum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return elm$core$Maybe$Just(
			A3(elm$core$List$foldl, elm$core$Basics$max, x, xs));
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var elm$core$List$minimum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return elm$core$Maybe$Just(
			A3(elm$core$List$foldl, elm$core$Basics$min, x, xs));
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var mdgriffith$elm_ui$Internal$Model$adjust = F3(
	function (size, height, vertical) {
		return {height: height / size, size: size, vertical: vertical};
	});
var mdgriffith$elm_ui$Internal$Model$convertAdjustment = function (adjustment) {
	var lines = _List_fromArray(
		[adjustment.capital, adjustment.baseline, adjustment.descender, adjustment.lowercase]);
	var lineHeight = 1.5;
	var normalDescender = (lineHeight - 1) / 2;
	var oldMiddle = lineHeight / 2;
	var descender = A2(
		elm$core$Maybe$withDefault,
		adjustment.descender,
		elm$core$List$minimum(lines));
	var newBaseline = A2(
		elm$core$Maybe$withDefault,
		adjustment.baseline,
		elm$core$List$minimum(
			A2(
				elm$core$List$filter,
				function (x) {
					return !_Utils_eq(x, descender);
				},
				lines)));
	var base = lineHeight;
	var ascender = A2(
		elm$core$Maybe$withDefault,
		adjustment.capital,
		elm$core$List$maximum(lines));
	var capitalSize = 1 / (ascender - newBaseline);
	var capitalVertical = 1 - ascender;
	var fullSize = 1 / (ascender - descender);
	var fullVertical = 1 - ascender;
	var newCapitalMiddle = ((ascender - newBaseline) / 2) + newBaseline;
	var newFullMiddle = ((ascender - descender) / 2) + descender;
	return {
		capital: A3(mdgriffith$elm_ui$Internal$Model$adjust, capitalSize, ascender - newBaseline, capitalVertical),
		full: A3(mdgriffith$elm_ui$Internal$Model$adjust, fullSize, ascender - descender, fullVertical)
	};
};
var mdgriffith$elm_ui$Internal$Model$fontAdjustmentRules = function (converted) {
	return _Utils_Tuple2(
		_List_fromArray(
			[
				_Utils_Tuple2('display', 'block')
			]),
		_List_fromArray(
			[
				_Utils_Tuple2('display', 'inline-block'),
				_Utils_Tuple2(
				'line-height',
				elm$core$String$fromFloat(converted.height)),
				_Utils_Tuple2(
				'vertical-align',
				elm$core$String$fromFloat(converted.vertical) + 'em'),
				_Utils_Tuple2(
				'font-size',
				elm$core$String$fromFloat(converted.size) + 'em')
			]));
};
var mdgriffith$elm_ui$Internal$Model$typefaceAdjustment = function (typefaces) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (face, found) {
				if (found.$ === 'Nothing') {
					if (face.$ === 'FontWith') {
						var _with = face.a;
						var _n2 = _with.adjustment;
						if (_n2.$ === 'Nothing') {
							return found;
						} else {
							var adjustment = _n2.a;
							return elm$core$Maybe$Just(
								_Utils_Tuple2(
									mdgriffith$elm_ui$Internal$Model$fontAdjustmentRules(
										function ($) {
											return $.full;
										}(
											mdgriffith$elm_ui$Internal$Model$convertAdjustment(adjustment))),
									mdgriffith$elm_ui$Internal$Model$fontAdjustmentRules(
										function ($) {
											return $.capital;
										}(
											mdgriffith$elm_ui$Internal$Model$convertAdjustment(adjustment)))));
						}
					} else {
						return found;
					}
				} else {
					return found;
				}
			}),
		elm$core$Maybe$Nothing,
		typefaces);
};
var mdgriffith$elm_ui$Internal$Model$renderTopLevelValues = function (rules) {
	var withImport = function (font) {
		if (font.$ === 'ImportFont') {
			var url = font.b;
			return elm$core$Maybe$Just('@import url(\'' + (url + '\');'));
		} else {
			return elm$core$Maybe$Nothing;
		}
	};
	var fontImports = function (_n2) {
		var name = _n2.a;
		var typefaces = _n2.b;
		var imports = A2(
			elm$core$String$join,
			'\n',
			A2(elm$core$List$filterMap, withImport, typefaces));
		return imports;
	};
	var allNames = A2(elm$core$List$map, elm$core$Tuple$first, rules);
	var fontAdjustments = function (_n1) {
		var name = _n1.a;
		var typefaces = _n1.b;
		var _n0 = mdgriffith$elm_ui$Internal$Model$typefaceAdjustment(typefaces);
		if (_n0.$ === 'Nothing') {
			return A2(
				elm$core$String$join,
				'',
				A2(
					elm$core$List$map,
					mdgriffith$elm_ui$Internal$Model$renderNullAdjustmentRule(name),
					allNames));
		} else {
			var adjustment = _n0.a;
			return A2(
				elm$core$String$join,
				'',
				A2(
					elm$core$List$map,
					A2(mdgriffith$elm_ui$Internal$Model$renderFontAdjustmentRule, name, adjustment),
					allNames));
		}
	};
	return _Utils_ap(
		A2(
			elm$core$String$join,
			'\n',
			A2(elm$core$List$map, fontImports, rules)),
		A2(
			elm$core$String$join,
			'\n',
			A2(elm$core$List$map, fontAdjustments, rules)));
};
var mdgriffith$elm_ui$Internal$Model$renderVariant = function (_var) {
	switch (_var.$) {
		case 'VariantActive':
			var name = _var.a;
			return '\"' + (name + '\"');
		case 'VariantOff':
			var name = _var.a;
			return '\"' + (name + '\" 0');
		default:
			var name = _var.a;
			var index = _var.b;
			return '\"' + (name + ('\" ' + elm$core$String$fromInt(index)));
	}
};
var mdgriffith$elm_ui$Internal$Model$renderVariants = function (typeface) {
	if (typeface.$ === 'FontWith') {
		var font = typeface.a;
		return elm$core$Maybe$Just(
			A2(
				elm$core$String$join,
				', ',
				A2(elm$core$List$map, mdgriffith$elm_ui$Internal$Model$renderVariant, font.variants)));
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var mdgriffith$elm_ui$Internal$Model$topLevelValue = function (rule) {
	if (rule.$ === 'FontFamily') {
		var name = rule.a;
		var typefaces = rule.b;
		return elm$core$Maybe$Just(
			_Utils_Tuple2(name, typefaces));
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var mdgriffith$elm_ui$Internal$Model$transformValue = function (transform) {
	switch (transform.$) {
		case 'Untransformed':
			return elm$core$Maybe$Nothing;
		case 'Moved':
			var _n1 = transform.a;
			var x = _n1.a;
			var y = _n1.b;
			var z = _n1.c;
			return elm$core$Maybe$Just(
				'translate3d(' + (elm$core$String$fromFloat(x) + ('px, ' + (elm$core$String$fromFloat(y) + ('px, ' + (elm$core$String$fromFloat(z) + 'px)'))))));
		default:
			var _n2 = transform.a;
			var tx = _n2.a;
			var ty = _n2.b;
			var tz = _n2.c;
			var _n3 = transform.b;
			var sx = _n3.a;
			var sy = _n3.b;
			var sz = _n3.c;
			var _n4 = transform.c;
			var ox = _n4.a;
			var oy = _n4.b;
			var oz = _n4.c;
			var angle = transform.d;
			var translate = 'translate3d(' + (elm$core$String$fromFloat(tx) + ('px, ' + (elm$core$String$fromFloat(ty) + ('px, ' + (elm$core$String$fromFloat(tz) + 'px)')))));
			var scale = 'scale3d(' + (elm$core$String$fromFloat(sx) + (', ' + (elm$core$String$fromFloat(sy) + (', ' + (elm$core$String$fromFloat(sz) + ')')))));
			var rotate = 'rotate3d(' + (elm$core$String$fromFloat(ox) + (', ' + (elm$core$String$fromFloat(oy) + (', ' + (elm$core$String$fromFloat(oz) + (', ' + (elm$core$String$fromFloat(angle) + 'rad)')))))));
			return elm$core$Maybe$Just(translate + (' ' + (scale + (' ' + rotate))));
	}
};
var mdgriffith$elm_ui$Internal$Model$toStyleSheetString = F2(
	function (options, stylesheet) {
		var renderStyle = F3(
			function (maybePseudo, selector, props) {
				if (maybePseudo.$ === 'Nothing') {
					return selector + ('{' + (A3(
						elm$core$List$foldl,
						mdgriffith$elm_ui$Internal$Model$renderProps(false),
						'',
						props) + '\n}'));
				} else {
					var pseudo = maybePseudo.a;
					switch (pseudo.$) {
						case 'Hover':
							var _n17 = options.hover;
							switch (_n17.$) {
								case 'NoHover':
									return '';
								case 'ForceHover':
									return selector + ('-hv {' + (A3(
										elm$core$List$foldl,
										mdgriffith$elm_ui$Internal$Model$renderProps(true),
										'',
										props) + '\n}'));
								default:
									return selector + ('-hv:hover {' + (A3(
										elm$core$List$foldl,
										mdgriffith$elm_ui$Internal$Model$renderProps(false),
										'',
										props) + '\n}'));
							}
						case 'Focus':
							var renderedProps = A3(
								elm$core$List$foldl,
								mdgriffith$elm_ui$Internal$Model$renderProps(false),
								'',
								props);
							return A2(
								elm$core$String$join,
								'\n',
								_List_fromArray(
									[selector + ('-fs:focus {' + (renderedProps + '\n}')), '.' + (mdgriffith$elm_ui$Internal$Style$classes.any + (':focus ~ ' + (selector + ('-fs:not(.focus)  {' + (renderedProps + '\n}'))))), '.' + (mdgriffith$elm_ui$Internal$Style$classes.any + (':focus ' + (selector + ('-fs  {' + (renderedProps + '\n}'))))), '.focusable-parent:focus ~ ' + ('.' + (mdgriffith$elm_ui$Internal$Style$classes.any + (' ' + (selector + ('-fs {' + (renderedProps + '\n}'))))))]));
						default:
							return selector + ('-act:active {' + (A3(
								elm$core$List$foldl,
								mdgriffith$elm_ui$Internal$Model$renderProps(false),
								'',
								props) + '\n}'));
					}
				}
			});
		var renderStyleRule = F2(
			function (rule, maybePseudo) {
				switch (rule.$) {
					case 'Style':
						var selector = rule.a;
						var props = rule.b;
						return A3(renderStyle, maybePseudo, selector, props);
					case 'Shadows':
						var name = rule.a;
						var prop = rule.b;
						return A3(
							renderStyle,
							maybePseudo,
							'.' + name,
							_List_fromArray(
								[
									A2(mdgriffith$elm_ui$Internal$Model$Property, 'box-shadow', prop)
								]));
					case 'Transparency':
						var name = rule.a;
						var transparency = rule.b;
						var opacity = A2(
							elm$core$Basics$max,
							0,
							A2(elm$core$Basics$min, 1, 1 - transparency));
						return A3(
							renderStyle,
							maybePseudo,
							'.' + name,
							_List_fromArray(
								[
									A2(
									mdgriffith$elm_ui$Internal$Model$Property,
									'opacity',
									elm$core$String$fromFloat(opacity))
								]));
					case 'FontSize':
						var i = rule.a;
						return A3(
							renderStyle,
							maybePseudo,
							'.font-size-' + elm$core$String$fromInt(i),
							_List_fromArray(
								[
									A2(
									mdgriffith$elm_ui$Internal$Model$Property,
									'font-size',
									elm$core$String$fromInt(i) + 'px')
								]));
					case 'FontFamily':
						var name = rule.a;
						var typefaces = rule.b;
						var features = A2(
							elm$core$String$join,
							', ',
							A2(elm$core$List$filterMap, mdgriffith$elm_ui$Internal$Model$renderVariants, typefaces));
						var families = _List_fromArray(
							[
								A2(
								mdgriffith$elm_ui$Internal$Model$Property,
								'font-family',
								A2(
									elm$core$String$join,
									', ',
									A2(elm$core$List$map, mdgriffith$elm_ui$Internal$Model$fontName, typefaces))),
								A2(mdgriffith$elm_ui$Internal$Model$Property, 'font-feature-settings', features),
								A2(
								mdgriffith$elm_ui$Internal$Model$Property,
								'font-variant',
								A2(elm$core$List$any, mdgriffith$elm_ui$Internal$Model$hasSmallCaps, typefaces) ? 'small-caps' : 'normal')
							]);
						return A2(
							elm$core$String$join,
							' ',
							_List_fromArray(
								[
									A3(renderStyle, maybePseudo, '.' + name, families)
								]));
					case 'Single':
						var _class = rule.a;
						var prop = rule.b;
						var val = rule.c;
						return A3(
							renderStyle,
							maybePseudo,
							'.' + _class,
							_List_fromArray(
								[
									A2(mdgriffith$elm_ui$Internal$Model$Property, prop, val)
								]));
					case 'Colored':
						var _class = rule.a;
						var prop = rule.b;
						var color = rule.c;
						return A3(
							renderStyle,
							maybePseudo,
							'.' + _class,
							_List_fromArray(
								[
									A2(
									mdgriffith$elm_ui$Internal$Model$Property,
									prop,
									mdgriffith$elm_ui$Internal$Model$formatColor(color))
								]));
					case 'SpacingStyle':
						var cls = rule.a;
						var x = rule.b;
						var y = rule.c;
						var yPx = elm$core$String$fromInt(y) + 'px';
						var xPx = elm$core$String$fromInt(x) + 'px';
						var single = '.' + mdgriffith$elm_ui$Internal$Style$classes.single;
						var row = '.' + mdgriffith$elm_ui$Internal$Style$classes.row;
						var wrappedRow = '.' + (mdgriffith$elm_ui$Internal$Style$classes.wrapped + row);
						var right = '.' + mdgriffith$elm_ui$Internal$Style$classes.alignRight;
						var paragraph = '.' + mdgriffith$elm_ui$Internal$Style$classes.paragraph;
						var page = '.' + mdgriffith$elm_ui$Internal$Style$classes.page;
						var left = '.' + mdgriffith$elm_ui$Internal$Style$classes.alignLeft;
						var halfY = elm$core$String$fromFloat(y / 2) + 'px';
						var halfX = elm$core$String$fromFloat(x / 2) + 'px';
						var column = '.' + mdgriffith$elm_ui$Internal$Style$classes.column;
						var _class = '.' + cls;
						var any = '.' + mdgriffith$elm_ui$Internal$Style$classes.any;
						return elm$core$String$concat(
							_List_fromArray(
								[
									A3(
									renderStyle,
									maybePseudo,
									_class + (row + (' > ' + (any + (' + ' + any)))),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'margin-left', xPx)
										])),
									A3(
									renderStyle,
									maybePseudo,
									_class + (wrappedRow + (' > ' + any)),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'margin', halfY + (' ' + halfX))
										])),
									A3(
									renderStyle,
									maybePseudo,
									_class + (column + (' > ' + (any + (' + ' + any)))),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'margin-top', yPx)
										])),
									A3(
									renderStyle,
									maybePseudo,
									_class + (page + (' > ' + (any + (' + ' + any)))),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'margin-top', yPx)
										])),
									A3(
									renderStyle,
									maybePseudo,
									_class + (page + (' > ' + left)),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'margin-right', xPx)
										])),
									A3(
									renderStyle,
									maybePseudo,
									_class + (page + (' > ' + right)),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'margin-left', xPx)
										])),
									A3(
									renderStyle,
									maybePseudo,
									_Utils_ap(_class, paragraph),
									_List_fromArray(
										[
											A2(
											mdgriffith$elm_ui$Internal$Model$Property,
											'line-height',
											'calc(1em + ' + (elm$core$String$fromInt(y) + 'px)'))
										])),
									A3(
									renderStyle,
									maybePseudo,
									'textarea' + _class,
									_List_fromArray(
										[
											A2(
											mdgriffith$elm_ui$Internal$Model$Property,
											'line-height',
											'calc(1em + ' + (elm$core$String$fromInt(y) + 'px)'))
										])),
									A3(
									renderStyle,
									maybePseudo,
									_class + (paragraph + (' > ' + left)),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'margin-right', xPx)
										])),
									A3(
									renderStyle,
									maybePseudo,
									_class + (paragraph + (' > ' + right)),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'margin-left', xPx)
										])),
									A3(
									renderStyle,
									maybePseudo,
									_class + (paragraph + '::after'),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'content', '\'\''),
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'display', 'block'),
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'height', '0'),
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'width', '0'),
											A2(
											mdgriffith$elm_ui$Internal$Model$Property,
											'margin-top',
											elm$core$String$fromInt((-1) * ((y / 2) | 0)) + 'px')
										])),
									A3(
									renderStyle,
									maybePseudo,
									_class + (paragraph + '::before'),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'content', '\'\''),
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'display', 'block'),
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'height', '0'),
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'width', '0'),
											A2(
											mdgriffith$elm_ui$Internal$Model$Property,
											'margin-bottom',
											elm$core$String$fromInt((-1) * ((y / 2) | 0)) + 'px')
										]))
								]));
					case 'PaddingStyle':
						var cls = rule.a;
						var top = rule.b;
						var right = rule.c;
						var bottom = rule.d;
						var left = rule.e;
						var _class = '.' + cls;
						return A3(
							renderStyle,
							maybePseudo,
							_class,
							_List_fromArray(
								[
									A2(
									mdgriffith$elm_ui$Internal$Model$Property,
									'padding',
									elm$core$String$fromInt(top) + ('px ' + (elm$core$String$fromInt(right) + ('px ' + (elm$core$String$fromInt(bottom) + ('px ' + (elm$core$String$fromInt(left) + 'px')))))))
								]));
					case 'BorderWidth':
						var cls = rule.a;
						var top = rule.b;
						var right = rule.c;
						var bottom = rule.d;
						var left = rule.e;
						var _class = '.' + cls;
						return A3(
							renderStyle,
							maybePseudo,
							_class,
							_List_fromArray(
								[
									A2(
									mdgriffith$elm_ui$Internal$Model$Property,
									'border-width',
									elm$core$String$fromInt(top) + ('px ' + (elm$core$String$fromInt(right) + ('px ' + (elm$core$String$fromInt(bottom) + ('px ' + (elm$core$String$fromInt(left) + 'px')))))))
								]));
					case 'GridTemplateStyle':
						var template = rule.a;
						var toGridLengthHelper = F3(
							function (minimum, maximum, x) {
								toGridLengthHelper:
								while (true) {
									switch (x.$) {
										case 'Px':
											var px = x.a;
											return elm$core$String$fromInt(px) + 'px';
										case 'Content':
											var _n2 = _Utils_Tuple2(minimum, maximum);
											if (_n2.a.$ === 'Nothing') {
												if (_n2.b.$ === 'Nothing') {
													var _n3 = _n2.a;
													var _n4 = _n2.b;
													return 'max-content';
												} else {
													var _n6 = _n2.a;
													var maxSize = _n2.b.a;
													return 'minmax(max-content, ' + (elm$core$String$fromInt(maxSize) + 'px)');
												}
											} else {
												if (_n2.b.$ === 'Nothing') {
													var minSize = _n2.a.a;
													var _n5 = _n2.b;
													return 'minmax(' + (elm$core$String$fromInt(minSize) + ('px, ' + 'max-content)'));
												} else {
													var minSize = _n2.a.a;
													var maxSize = _n2.b.a;
													return 'minmax(' + (elm$core$String$fromInt(minSize) + ('px, ' + (elm$core$String$fromInt(maxSize) + 'px)')));
												}
											}
										case 'Fill':
											var i = x.a;
											var _n7 = _Utils_Tuple2(minimum, maximum);
											if (_n7.a.$ === 'Nothing') {
												if (_n7.b.$ === 'Nothing') {
													var _n8 = _n7.a;
													var _n9 = _n7.b;
													return elm$core$String$fromInt(i) + 'fr';
												} else {
													var _n11 = _n7.a;
													var maxSize = _n7.b.a;
													return 'minmax(max-content, ' + (elm$core$String$fromInt(maxSize) + 'px)');
												}
											} else {
												if (_n7.b.$ === 'Nothing') {
													var minSize = _n7.a.a;
													var _n10 = _n7.b;
													return 'minmax(' + (elm$core$String$fromInt(minSize) + ('px, ' + (elm$core$String$fromInt(i) + ('fr' + 'fr)'))));
												} else {
													var minSize = _n7.a.a;
													var maxSize = _n7.b.a;
													return 'minmax(' + (elm$core$String$fromInt(minSize) + ('px, ' + (elm$core$String$fromInt(maxSize) + 'px)')));
												}
											}
										case 'Min':
											var m = x.a;
											var len = x.b;
											var $temp$minimum = elm$core$Maybe$Just(m),
												$temp$maximum = maximum,
												$temp$x = len;
											minimum = $temp$minimum;
											maximum = $temp$maximum;
											x = $temp$x;
											continue toGridLengthHelper;
										default:
											var m = x.a;
											var len = x.b;
											var $temp$minimum = minimum,
												$temp$maximum = elm$core$Maybe$Just(m),
												$temp$x = len;
											minimum = $temp$minimum;
											maximum = $temp$maximum;
											x = $temp$x;
											continue toGridLengthHelper;
									}
								}
							});
						var toGridLength = function (x) {
							return A3(toGridLengthHelper, elm$core$Maybe$Nothing, elm$core$Maybe$Nothing, x);
						};
						var xSpacing = toGridLength(template.spacing.a);
						var ySpacing = toGridLength(template.spacing.b);
						var rows = function (x) {
							return 'grid-template-rows: ' + (x + ';');
						}(
							A2(
								elm$core$String$join,
								' ',
								A2(elm$core$List$map, toGridLength, template.rows)));
						var msRows = function (x) {
							return '-ms-grid-rows: ' + (x + ';');
						}(
							A2(
								elm$core$String$join,
								ySpacing,
								A2(elm$core$List$map, toGridLength, template.columns)));
						var msColumns = function (x) {
							return '-ms-grid-columns: ' + (x + ';');
						}(
							A2(
								elm$core$String$join,
								ySpacing,
								A2(elm$core$List$map, toGridLength, template.columns)));
						var gapY = 'grid-row-gap:' + (toGridLength(template.spacing.b) + ';');
						var gapX = 'grid-column-gap:' + (toGridLength(template.spacing.a) + ';');
						var columns = function (x) {
							return 'grid-template-columns: ' + (x + ';');
						}(
							A2(
								elm$core$String$join,
								' ',
								A2(elm$core$List$map, toGridLength, template.columns)));
						var _class = '.grid-rows-' + (A2(
							elm$core$String$join,
							'-',
							A2(elm$core$List$map, mdgriffith$elm_ui$Internal$Model$lengthClassName, template.rows)) + ('-cols-' + (A2(
							elm$core$String$join,
							'-',
							A2(elm$core$List$map, mdgriffith$elm_ui$Internal$Model$lengthClassName, template.columns)) + ('-space-x-' + (mdgriffith$elm_ui$Internal$Model$lengthClassName(template.spacing.a) + ('-space-y-' + mdgriffith$elm_ui$Internal$Model$lengthClassName(template.spacing.b)))))));
						var modernGrid = _class + ('{' + (columns + (rows + (gapX + (gapY + '}')))));
						var supports = '@supports (display:grid) {' + (modernGrid + '}');
						var base = _class + ('{' + (msColumns + (msRows + '}')));
						return _Utils_ap(base, supports);
					case 'GridPosition':
						var position = rule.a;
						var msPosition = A2(
							elm$core$String$join,
							' ',
							_List_fromArray(
								[
									'-ms-grid-row: ' + (elm$core$String$fromInt(position.row) + ';'),
									'-ms-grid-row-span: ' + (elm$core$String$fromInt(position.height) + ';'),
									'-ms-grid-column: ' + (elm$core$String$fromInt(position.col) + ';'),
									'-ms-grid-column-span: ' + (elm$core$String$fromInt(position.width) + ';')
								]));
						var modernPosition = A2(
							elm$core$String$join,
							' ',
							_List_fromArray(
								[
									'grid-row: ' + (elm$core$String$fromInt(position.row) + (' / ' + (elm$core$String$fromInt(position.row + position.height) + ';'))),
									'grid-column: ' + (elm$core$String$fromInt(position.col) + (' / ' + (elm$core$String$fromInt(position.col + position.width) + ';')))
								]));
						var _class = '.grid-pos-' + (elm$core$String$fromInt(position.row) + ('-' + (elm$core$String$fromInt(position.col) + ('-' + (elm$core$String$fromInt(position.width) + ('-' + elm$core$String$fromInt(position.height)))))));
						var modernGrid = _class + ('{' + (modernPosition + '}'));
						var supports = '@supports (display:grid) {' + (modernGrid + '}');
						var base = _class + ('{' + (msPosition + '}'));
						return _Utils_ap(base, supports);
					case 'PseudoSelector':
						var _class = rule.a;
						var styles = rule.b;
						var renderPseudoRule = function (style) {
							return A2(
								renderStyleRule,
								style,
								elm$core$Maybe$Just(_class));
						};
						return A2(
							elm$core$String$join,
							' ',
							A2(elm$core$List$map, renderPseudoRule, styles));
					default:
						var transform = rule.a;
						var val = mdgriffith$elm_ui$Internal$Model$transformValue(transform);
						var _class = mdgriffith$elm_ui$Internal$Model$transformClass(transform);
						var _n12 = _Utils_Tuple2(_class, val);
						if ((_n12.a.$ === 'Just') && (_n12.b.$ === 'Just')) {
							var cls = _n12.a.a;
							var v = _n12.b.a;
							return A3(
								renderStyle,
								maybePseudo,
								'.' + cls,
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Model$Property, 'transform', v)
									]));
						} else {
							return '';
						}
				}
			});
		var combine = F2(
			function (style, rendered) {
				return {
					rules: _Utils_ap(
						rendered.rules,
						A2(renderStyleRule, style, elm$core$Maybe$Nothing)),
					topLevel: function () {
						var _n14 = mdgriffith$elm_ui$Internal$Model$topLevelValue(style);
						if (_n14.$ === 'Nothing') {
							return rendered.topLevel;
						} else {
							var topLevel = _n14.a;
							return A2(elm$core$List$cons, topLevel, rendered.topLevel);
						}
					}()
				};
			});
		var _n13 = A3(
			elm$core$List$foldl,
			combine,
			{rules: '', topLevel: _List_Nil},
			stylesheet);
		var topLevel = _n13.topLevel;
		var rules = _n13.rules;
		return _Utils_ap(
			mdgriffith$elm_ui$Internal$Model$renderTopLevelValues(topLevel),
			rules);
	});
var mdgriffith$elm_ui$Internal$Model$toStyleSheet = F2(
	function (options, styleSheet) {
		return A3(
			elm$virtual_dom$VirtualDom$node,
			'style',
			_List_Nil,
			_List_fromArray(
				[
					elm$virtual_dom$VirtualDom$text(
					A2(mdgriffith$elm_ui$Internal$Model$toStyleSheetString, options, styleSheet))
				]));
	});
var mdgriffith$elm_ui$Internal$Model$embedKeyed = F4(
	function (_static, opts, styles, children) {
		return _static ? A2(
			elm$core$List$cons,
			_Utils_Tuple2('static-stylesheet', mdgriffith$elm_ui$Internal$Model$staticRoot),
			A2(
				elm$core$List$cons,
				_Utils_Tuple2(
					'dynamic-stylesheet',
					A2(
						mdgriffith$elm_ui$Internal$Model$toStyleSheet,
						opts,
						A3(
							elm$core$List$foldl,
							mdgriffith$elm_ui$Internal$Model$reduceStyles,
							_Utils_Tuple2(
								elm$core$Set$empty,
								_List_fromArray(
									[
										mdgriffith$elm_ui$Internal$Model$renderFocusStyle(opts.focus)
									])),
							styles).b)),
				children)) : A2(
			elm$core$List$cons,
			_Utils_Tuple2(
				'dynamic-stylesheet',
				A2(
					mdgriffith$elm_ui$Internal$Model$toStyleSheet,
					opts,
					A3(
						elm$core$List$foldl,
						mdgriffith$elm_ui$Internal$Model$reduceStyles,
						_Utils_Tuple2(
							elm$core$Set$empty,
							_List_fromArray(
								[
									mdgriffith$elm_ui$Internal$Model$renderFocusStyle(opts.focus)
								])),
						styles).b)),
			children);
	});
var mdgriffith$elm_ui$Internal$Model$embedWith = F4(
	function (_static, opts, styles, children) {
		return _static ? A2(
			elm$core$List$cons,
			mdgriffith$elm_ui$Internal$Model$staticRoot,
			A2(
				elm$core$List$cons,
				A2(
					mdgriffith$elm_ui$Internal$Model$toStyleSheet,
					opts,
					A3(
						elm$core$List$foldl,
						mdgriffith$elm_ui$Internal$Model$reduceStyles,
						_Utils_Tuple2(
							elm$core$Set$empty,
							_List_fromArray(
								[
									mdgriffith$elm_ui$Internal$Model$renderFocusStyle(opts.focus)
								])),
						styles).b),
				children)) : A2(
			elm$core$List$cons,
			A2(
				mdgriffith$elm_ui$Internal$Model$toStyleSheet,
				opts,
				A3(
					elm$core$List$foldl,
					mdgriffith$elm_ui$Internal$Model$reduceStyles,
					_Utils_Tuple2(
						elm$core$Set$empty,
						_List_fromArray(
							[
								mdgriffith$elm_ui$Internal$Model$renderFocusStyle(opts.focus)
							])),
					styles).b),
			children);
	});
var mdgriffith$elm_ui$Internal$Model$finalizeNode = F6(
	function (has, node, attributes, children, embedMode, parentContext) {
		var createNode = F2(
			function (nodeName, attrs) {
				if (children.$ === 'Keyed') {
					var keyed = children.a;
					return A3(
						elm$virtual_dom$VirtualDom$keyedNode,
						nodeName,
						attrs,
						function () {
							switch (embedMode.$) {
								case 'NoStyleSheet':
									return keyed;
								case 'OnlyDynamic':
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4(mdgriffith$elm_ui$Internal$Model$embedKeyed, false, opts, styles, keyed);
								default:
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4(mdgriffith$elm_ui$Internal$Model$embedKeyed, true, opts, styles, keyed);
							}
						}());
				} else {
					var unkeyed = children.a;
					return A2(
						function () {
							switch (nodeName) {
								case 'div':
									return elm$html$Html$div;
								case 'p':
									return elm$html$Html$p;
								default:
									return elm$virtual_dom$VirtualDom$node(nodeName);
							}
						}(),
						attrs,
						function () {
							switch (embedMode.$) {
								case 'NoStyleSheet':
									return unkeyed;
								case 'OnlyDynamic':
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4(mdgriffith$elm_ui$Internal$Model$embedWith, false, opts, styles, unkeyed);
								default:
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4(mdgriffith$elm_ui$Internal$Model$embedWith, true, opts, styles, unkeyed);
							}
						}());
				}
			});
		var html = function () {
			switch (node.$) {
				case 'Generic':
					return A2(createNode, 'div', attributes);
				case 'NodeName':
					var nodeName = node.a;
					return A2(createNode, nodeName, attributes);
				default:
					var nodeName = node.a;
					var internal = node.b;
					return A3(
						elm$virtual_dom$VirtualDom$node,
						nodeName,
						attributes,
						_List_fromArray(
							[
								A2(
								createNode,
								internal,
								_List_fromArray(
									[
										elm$html$Html$Attributes$class(mdgriffith$elm_ui$Internal$Style$classes.any + (' ' + mdgriffith$elm_ui$Internal$Style$classes.single))
									]))
							]));
			}
		}();
		switch (parentContext.$) {
			case 'AsRow':
				return (A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$widthFill, has) && (!A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$widthBetween, has))) ? html : (A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$alignRight, has) ? A2(
					elm$html$Html$u,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class(
							A2(
								elm$core$String$join,
								' ',
								_List_fromArray(
									[mdgriffith$elm_ui$Internal$Style$classes.any, mdgriffith$elm_ui$Internal$Style$classes.single, mdgriffith$elm_ui$Internal$Style$classes.container, mdgriffith$elm_ui$Internal$Style$classes.contentCenterY, mdgriffith$elm_ui$Internal$Style$classes.alignContainerRight])))
						]),
					_List_fromArray(
						[html])) : (A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$centerX, has) ? A2(
					elm$html$Html$s,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class(
							A2(
								elm$core$String$join,
								' ',
								_List_fromArray(
									[mdgriffith$elm_ui$Internal$Style$classes.any, mdgriffith$elm_ui$Internal$Style$classes.single, mdgriffith$elm_ui$Internal$Style$classes.container, mdgriffith$elm_ui$Internal$Style$classes.contentCenterY, mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterX])))
						]),
					_List_fromArray(
						[html])) : html));
			case 'AsColumn':
				return (A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$heightFill, has) && (!A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$heightBetween, has))) ? html : (A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$centerY, has) ? A2(
					elm$html$Html$s,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class(
							A2(
								elm$core$String$join,
								' ',
								_List_fromArray(
									[mdgriffith$elm_ui$Internal$Style$classes.any, mdgriffith$elm_ui$Internal$Style$classes.single, mdgriffith$elm_ui$Internal$Style$classes.container, mdgriffith$elm_ui$Internal$Style$classes.alignContainerCenterY])))
						]),
					_List_fromArray(
						[html])) : (A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$alignBottom, has) ? A2(
					elm$html$Html$u,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class(
							A2(
								elm$core$String$join,
								' ',
								_List_fromArray(
									[mdgriffith$elm_ui$Internal$Style$classes.any, mdgriffith$elm_ui$Internal$Style$classes.single, mdgriffith$elm_ui$Internal$Style$classes.container, mdgriffith$elm_ui$Internal$Style$classes.alignContainerBottom])))
						]),
					_List_fromArray(
						[html])) : html));
			default:
				return html;
		}
	});
var elm$html$Html$text = elm$virtual_dom$VirtualDom$text;
var mdgriffith$elm_ui$Internal$Model$textElement = function (str) {
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class(
				A2(
					elm$core$String$join,
					' ',
					_List_fromArray(
						[mdgriffith$elm_ui$Internal$Style$classes.any, mdgriffith$elm_ui$Internal$Style$classes.text, mdgriffith$elm_ui$Internal$Style$classes.widthContent, mdgriffith$elm_ui$Internal$Style$classes.heightContent])))
			]),
		_List_fromArray(
			[
				elm$html$Html$text(str)
			]));
};
var mdgriffith$elm_ui$Internal$Model$textElementFill = function (str) {
	return A3(
		elm$virtual_dom$VirtualDom$node,
		'div',
		_List_fromArray(
			[
				elm$html$Html$Attributes$class(
				A2(
					elm$core$String$join,
					' ',
					_List_fromArray(
						[mdgriffith$elm_ui$Internal$Style$classes.any, mdgriffith$elm_ui$Internal$Style$classes.text, mdgriffith$elm_ui$Internal$Style$classes.widthFill, mdgriffith$elm_ui$Internal$Style$classes.heightFill])))
			]),
		_List_fromArray(
			[
				elm$virtual_dom$VirtualDom$text(str)
			]));
};
var mdgriffith$elm_ui$Internal$Model$createElement = F3(
	function (context, children, rendered) {
		var gatherKeyed = F2(
			function (_n8, _n9) {
				var key = _n8.a;
				var child = _n8.b;
				var htmls = _n9.a;
				var existingStyles = _n9.b;
				switch (child.$) {
					case 'Unstyled':
						var html = child.a;
						return _Utils_eq(context, mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								elm$core$List$cons,
								_Utils_Tuple2(
									key,
									html(context)),
								htmls),
							existingStyles) : _Utils_Tuple2(
							A2(
								elm$core$List$cons,
								_Utils_Tuple2(
									key,
									html(context)),
								htmls),
							existingStyles);
					case 'Styled':
						var styled = child.a;
						return _Utils_eq(context, mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								elm$core$List$cons,
								_Utils_Tuple2(
									key,
									A2(styled.html, mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context)),
								htmls),
							elm$core$List$isEmpty(existingStyles) ? styled.styles : _Utils_ap(styled.styles, existingStyles)) : _Utils_Tuple2(
							A2(
								elm$core$List$cons,
								_Utils_Tuple2(
									key,
									A2(styled.html, mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context)),
								htmls),
							elm$core$List$isEmpty(existingStyles) ? styled.styles : _Utils_ap(styled.styles, existingStyles));
					case 'Text':
						var str = child.a;
						return _Utils_Tuple2(
							A2(
								elm$core$List$cons,
								_Utils_Tuple2(
									key,
									_Utils_eq(context, mdgriffith$elm_ui$Internal$Model$asEl) ? mdgriffith$elm_ui$Internal$Model$textElementFill(str) : mdgriffith$elm_ui$Internal$Model$textElement(str)),
								htmls),
							existingStyles);
					default:
						return _Utils_Tuple2(htmls, existingStyles);
				}
			});
		var gather = F2(
			function (child, _n6) {
				var htmls = _n6.a;
				var existingStyles = _n6.b;
				switch (child.$) {
					case 'Unstyled':
						var html = child.a;
						return _Utils_eq(context, mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								elm$core$List$cons,
								html(context),
								htmls),
							existingStyles) : _Utils_Tuple2(
							A2(
								elm$core$List$cons,
								html(context),
								htmls),
							existingStyles);
					case 'Styled':
						var styled = child.a;
						return _Utils_eq(context, mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								elm$core$List$cons,
								A2(styled.html, mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context),
								htmls),
							elm$core$List$isEmpty(existingStyles) ? styled.styles : _Utils_ap(styled.styles, existingStyles)) : _Utils_Tuple2(
							A2(
								elm$core$List$cons,
								A2(styled.html, mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context),
								htmls),
							elm$core$List$isEmpty(existingStyles) ? styled.styles : _Utils_ap(styled.styles, existingStyles));
					case 'Text':
						var str = child.a;
						return _Utils_Tuple2(
							A2(
								elm$core$List$cons,
								_Utils_eq(context, mdgriffith$elm_ui$Internal$Model$asEl) ? mdgriffith$elm_ui$Internal$Model$textElementFill(str) : mdgriffith$elm_ui$Internal$Model$textElement(str),
								htmls),
							existingStyles);
					default:
						return _Utils_Tuple2(htmls, existingStyles);
				}
			});
		if (children.$ === 'Keyed') {
			var keyedChildren = children.a;
			var _n1 = A3(
				elm$core$List$foldr,
				gatherKeyed,
				_Utils_Tuple2(_List_Nil, _List_Nil),
				keyedChildren);
			var keyed = _n1.a;
			var styles = _n1.b;
			var newStyles = elm$core$List$isEmpty(styles) ? rendered.styles : _Utils_ap(rendered.styles, styles);
			if (!newStyles.b) {
				return mdgriffith$elm_ui$Internal$Model$Unstyled(
					A5(
						mdgriffith$elm_ui$Internal$Model$finalizeNode,
						rendered.has,
						rendered.node,
						rendered.attributes,
						mdgriffith$elm_ui$Internal$Model$Keyed(
							A3(mdgriffith$elm_ui$Internal$Model$addKeyedChildren, 'nearby-element-pls', keyed, rendered.children)),
						mdgriffith$elm_ui$Internal$Model$NoStyleSheet));
			} else {
				var allStyles = newStyles;
				return mdgriffith$elm_ui$Internal$Model$Styled(
					{
						html: A4(
							mdgriffith$elm_ui$Internal$Model$finalizeNode,
							rendered.has,
							rendered.node,
							rendered.attributes,
							mdgriffith$elm_ui$Internal$Model$Keyed(
								A3(mdgriffith$elm_ui$Internal$Model$addKeyedChildren, 'nearby-element-pls', keyed, rendered.children))),
						styles: allStyles
					});
			}
		} else {
			var unkeyedChildren = children.a;
			var _n3 = A3(
				elm$core$List$foldr,
				gather,
				_Utils_Tuple2(_List_Nil, _List_Nil),
				unkeyedChildren);
			var unkeyed = _n3.a;
			var styles = _n3.b;
			var newStyles = elm$core$List$isEmpty(styles) ? rendered.styles : _Utils_ap(rendered.styles, styles);
			if (!newStyles.b) {
				return mdgriffith$elm_ui$Internal$Model$Unstyled(
					A5(
						mdgriffith$elm_ui$Internal$Model$finalizeNode,
						rendered.has,
						rendered.node,
						rendered.attributes,
						mdgriffith$elm_ui$Internal$Model$Unkeyed(
							A2(mdgriffith$elm_ui$Internal$Model$addChildren, unkeyed, rendered.children)),
						mdgriffith$elm_ui$Internal$Model$NoStyleSheet));
			} else {
				var allStyles = newStyles;
				return mdgriffith$elm_ui$Internal$Model$Styled(
					{
						html: A4(
							mdgriffith$elm_ui$Internal$Model$finalizeNode,
							rendered.has,
							rendered.node,
							rendered.attributes,
							mdgriffith$elm_ui$Internal$Model$Unkeyed(
								A2(mdgriffith$elm_ui$Internal$Model$addChildren, unkeyed, rendered.children))),
						styles: allStyles
					});
			}
		}
	});
var elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var elm$core$Bitwise$or = _Bitwise_or;
var mdgriffith$elm_ui$Internal$Flag$add = F2(
	function (myFlag, _n0) {
		var one = _n0.a;
		var two = _n0.b;
		if (myFlag.$ === 'Flag') {
			var first = myFlag.a;
			return A2(mdgriffith$elm_ui$Internal$Flag$Field, first | one, two);
		} else {
			var second = myFlag.a;
			return A2(mdgriffith$elm_ui$Internal$Flag$Field, one, second | two);
		}
	});
var mdgriffith$elm_ui$Internal$Flag$height = mdgriffith$elm_ui$Internal$Flag$flag(7);
var mdgriffith$elm_ui$Internal$Flag$heightContent = mdgriffith$elm_ui$Internal$Flag$flag(36);
var mdgriffith$elm_ui$Internal$Flag$merge = F2(
	function (_n0, _n1) {
		var one = _n0.a;
		var two = _n0.b;
		var three = _n1.a;
		var four = _n1.b;
		return A2(mdgriffith$elm_ui$Internal$Flag$Field, one | three, two | four);
	});
var mdgriffith$elm_ui$Internal$Flag$width = mdgriffith$elm_ui$Internal$Flag$flag(6);
var mdgriffith$elm_ui$Internal$Flag$widthContent = mdgriffith$elm_ui$Internal$Flag$flag(38);
var mdgriffith$elm_ui$Internal$Flag$xAlign = mdgriffith$elm_ui$Internal$Flag$flag(30);
var mdgriffith$elm_ui$Internal$Flag$yAlign = mdgriffith$elm_ui$Internal$Flag$flag(29);
var mdgriffith$elm_ui$Internal$Model$Embedded = F2(
	function (a, b) {
		return {$: 'Embedded', a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Model$NodeName = function (a) {
	return {$: 'NodeName', a: a};
};
var mdgriffith$elm_ui$Internal$Model$Single = F3(
	function (a, b, c) {
		return {$: 'Single', a: a, b: b, c: c};
	});
var mdgriffith$elm_ui$Internal$Model$Transform = function (a) {
	return {$: 'Transform', a: a};
};
var mdgriffith$elm_ui$Internal$Model$ChildrenBehind = function (a) {
	return {$: 'ChildrenBehind', a: a};
};
var mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront = F2(
	function (a, b) {
		return {$: 'ChildrenBehindAndInFront', a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Model$ChildrenInFront = function (a) {
	return {$: 'ChildrenInFront', a: a};
};
var mdgriffith$elm_ui$Internal$Model$nearbyElement = F2(
	function (location, elem) {
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class(
					function () {
						switch (location.$) {
							case 'Above':
								return A2(
									elm$core$String$join,
									' ',
									_List_fromArray(
										[mdgriffith$elm_ui$Internal$Style$classes.nearby, mdgriffith$elm_ui$Internal$Style$classes.single, mdgriffith$elm_ui$Internal$Style$classes.above]));
							case 'Below':
								return A2(
									elm$core$String$join,
									' ',
									_List_fromArray(
										[mdgriffith$elm_ui$Internal$Style$classes.nearby, mdgriffith$elm_ui$Internal$Style$classes.single, mdgriffith$elm_ui$Internal$Style$classes.below]));
							case 'OnRight':
								return A2(
									elm$core$String$join,
									' ',
									_List_fromArray(
										[mdgriffith$elm_ui$Internal$Style$classes.nearby, mdgriffith$elm_ui$Internal$Style$classes.single, mdgriffith$elm_ui$Internal$Style$classes.onRight]));
							case 'OnLeft':
								return A2(
									elm$core$String$join,
									' ',
									_List_fromArray(
										[mdgriffith$elm_ui$Internal$Style$classes.nearby, mdgriffith$elm_ui$Internal$Style$classes.single, mdgriffith$elm_ui$Internal$Style$classes.onLeft]));
							case 'InFront':
								return A2(
									elm$core$String$join,
									' ',
									_List_fromArray(
										[mdgriffith$elm_ui$Internal$Style$classes.nearby, mdgriffith$elm_ui$Internal$Style$classes.single, mdgriffith$elm_ui$Internal$Style$classes.inFront]));
							default:
								return A2(
									elm$core$String$join,
									' ',
									_List_fromArray(
										[mdgriffith$elm_ui$Internal$Style$classes.nearby, mdgriffith$elm_ui$Internal$Style$classes.single, mdgriffith$elm_ui$Internal$Style$classes.behind]));
						}
					}())
				]),
			_List_fromArray(
				[
					function () {
					switch (elem.$) {
						case 'Empty':
							return elm$virtual_dom$VirtualDom$text('');
						case 'Text':
							var str = elem.a;
							return mdgriffith$elm_ui$Internal$Model$textElement(str);
						case 'Unstyled':
							var html = elem.a;
							return html(mdgriffith$elm_ui$Internal$Model$asEl);
						default:
							var styled = elem.a;
							return A2(styled.html, mdgriffith$elm_ui$Internal$Model$NoStyleSheet, mdgriffith$elm_ui$Internal$Model$asEl);
					}
				}()
				]));
	});
var mdgriffith$elm_ui$Internal$Model$addNearbyElement = F3(
	function (location, elem, existing) {
		var nearby = A2(mdgriffith$elm_ui$Internal$Model$nearbyElement, location, elem);
		switch (existing.$) {
			case 'NoNearbyChildren':
				if (location.$ === 'Behind') {
					return mdgriffith$elm_ui$Internal$Model$ChildrenBehind(
						_List_fromArray(
							[nearby]));
				} else {
					return mdgriffith$elm_ui$Internal$Model$ChildrenInFront(
						_List_fromArray(
							[nearby]));
				}
			case 'ChildrenBehind':
				var existingBehind = existing.a;
				if (location.$ === 'Behind') {
					return mdgriffith$elm_ui$Internal$Model$ChildrenBehind(
						A2(elm$core$List$cons, nearby, existingBehind));
				} else {
					return A2(
						mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						existingBehind,
						_List_fromArray(
							[nearby]));
				}
			case 'ChildrenInFront':
				var existingInFront = existing.a;
				if (location.$ === 'Behind') {
					return A2(
						mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						_List_fromArray(
							[nearby]),
						existingInFront);
				} else {
					return mdgriffith$elm_ui$Internal$Model$ChildrenInFront(
						A2(elm$core$List$cons, nearby, existingInFront));
				}
			default:
				var existingBehind = existing.a;
				var existingInFront = existing.b;
				if (location.$ === 'Behind') {
					return A2(
						mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						A2(elm$core$List$cons, nearby, existingBehind),
						existingInFront);
				} else {
					return A2(
						mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						existingBehind,
						A2(elm$core$List$cons, nearby, existingInFront));
				}
		}
	});
var mdgriffith$elm_ui$Internal$Model$addNodeName = F2(
	function (newNode, old) {
		switch (old.$) {
			case 'Generic':
				return mdgriffith$elm_ui$Internal$Model$NodeName(newNode);
			case 'NodeName':
				var name = old.a;
				return A2(mdgriffith$elm_ui$Internal$Model$Embedded, name, newNode);
			default:
				var x = old.a;
				var y = old.b;
				return A2(mdgriffith$elm_ui$Internal$Model$Embedded, x, y);
		}
	});
var mdgriffith$elm_ui$Internal$Model$alignXName = function (align) {
	switch (align.$) {
		case 'Left':
			return mdgriffith$elm_ui$Internal$Style$classes.alignedHorizontally + (' ' + mdgriffith$elm_ui$Internal$Style$classes.alignLeft);
		case 'Right':
			return mdgriffith$elm_ui$Internal$Style$classes.alignedHorizontally + (' ' + mdgriffith$elm_ui$Internal$Style$classes.alignRight);
		default:
			return mdgriffith$elm_ui$Internal$Style$classes.alignedHorizontally + (' ' + mdgriffith$elm_ui$Internal$Style$classes.alignCenterX);
	}
};
var mdgriffith$elm_ui$Internal$Model$alignYName = function (align) {
	switch (align.$) {
		case 'Top':
			return mdgriffith$elm_ui$Internal$Style$classes.alignedVertically + (' ' + mdgriffith$elm_ui$Internal$Style$classes.alignTop);
		case 'Bottom':
			return mdgriffith$elm_ui$Internal$Style$classes.alignedVertically + (' ' + mdgriffith$elm_ui$Internal$Style$classes.alignBottom);
		default:
			return mdgriffith$elm_ui$Internal$Style$classes.alignedVertically + (' ' + mdgriffith$elm_ui$Internal$Style$classes.alignCenterY);
	}
};
var mdgriffith$elm_ui$Internal$Model$FullTransform = F4(
	function (a, b, c, d) {
		return {$: 'FullTransform', a: a, b: b, c: c, d: d};
	});
var mdgriffith$elm_ui$Internal$Model$Moved = function (a) {
	return {$: 'Moved', a: a};
};
var mdgriffith$elm_ui$Internal$Model$composeTransformation = F2(
	function (transform, component) {
		switch (transform.$) {
			case 'Untransformed':
				switch (component.$) {
					case 'MoveX':
						var x = component.a;
						return mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(x, 0, 0));
					case 'MoveY':
						var y = component.a;
						return mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(0, y, 0));
					case 'MoveZ':
						var z = component.a;
						return mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(0, 0, z));
					case 'MoveXYZ':
						var xyz = component.a;
						return mdgriffith$elm_ui$Internal$Model$Moved(xyz);
					case 'Rotate':
						var xyz = component.a;
						var angle = component.b;
						return A4(
							mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(0, 0, 0),
							_Utils_Tuple3(1, 1, 1),
							xyz,
							angle);
					default:
						var xyz = component.a;
						return A4(
							mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(0, 0, 0),
							xyz,
							_Utils_Tuple3(0, 0, 1),
							0);
				}
			case 'Moved':
				var moved = transform.a;
				var x = moved.a;
				var y = moved.b;
				var z = moved.c;
				switch (component.$) {
					case 'MoveX':
						var newX = component.a;
						return mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(newX, y, z));
					case 'MoveY':
						var newY = component.a;
						return mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(x, newY, z));
					case 'MoveZ':
						var newZ = component.a;
						return mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(x, y, newZ));
					case 'MoveXYZ':
						var xyz = component.a;
						return mdgriffith$elm_ui$Internal$Model$Moved(xyz);
					case 'Rotate':
						var xyz = component.a;
						var angle = component.b;
						return A4(
							mdgriffith$elm_ui$Internal$Model$FullTransform,
							moved,
							_Utils_Tuple3(1, 1, 1),
							xyz,
							angle);
					default:
						var scale = component.a;
						return A4(
							mdgriffith$elm_ui$Internal$Model$FullTransform,
							moved,
							scale,
							_Utils_Tuple3(0, 0, 1),
							0);
				}
			default:
				var moved = transform.a;
				var x = moved.a;
				var y = moved.b;
				var z = moved.c;
				var scaled = transform.b;
				var origin = transform.c;
				var angle = transform.d;
				switch (component.$) {
					case 'MoveX':
						var newX = component.a;
						return A4(
							mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(newX, y, z),
							scaled,
							origin,
							angle);
					case 'MoveY':
						var newY = component.a;
						return A4(
							mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(x, newY, z),
							scaled,
							origin,
							angle);
					case 'MoveZ':
						var newZ = component.a;
						return A4(
							mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(x, y, newZ),
							scaled,
							origin,
							angle);
					case 'MoveXYZ':
						var newMove = component.a;
						return A4(mdgriffith$elm_ui$Internal$Model$FullTransform, newMove, scaled, origin, angle);
					case 'Rotate':
						var newOrigin = component.a;
						var newAngle = component.b;
						return A4(mdgriffith$elm_ui$Internal$Model$FullTransform, moved, scaled, newOrigin, newAngle);
					default:
						var newScale = component.a;
						return A4(mdgriffith$elm_ui$Internal$Model$FullTransform, moved, newScale, origin, angle);
				}
		}
	});
var mdgriffith$elm_ui$Internal$Model$renderHeight = function (h) {
	switch (h.$) {
		case 'Px':
			var px = h.a;
			var val = elm$core$String$fromInt(px);
			var name = 'height-px-' + val;
			return _Utils_Tuple3(
				mdgriffith$elm_ui$Internal$Flag$none,
				name,
				_List_fromArray(
					[
						A3(mdgriffith$elm_ui$Internal$Model$Single, name, 'height', val + 'px')
					]));
		case 'Content':
			return _Utils_Tuple3(
				A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$heightContent, mdgriffith$elm_ui$Internal$Flag$none),
				mdgriffith$elm_ui$Internal$Style$classes.heightContent,
				_List_Nil);
		case 'Fill':
			var portion = h.a;
			return (portion === 1) ? _Utils_Tuple3(
				A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$heightFill, mdgriffith$elm_ui$Internal$Flag$none),
				mdgriffith$elm_ui$Internal$Style$classes.heightFill,
				_List_Nil) : _Utils_Tuple3(
				A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$heightFill, mdgriffith$elm_ui$Internal$Flag$none),
				mdgriffith$elm_ui$Internal$Style$classes.heightFillPortion + (' height-fill-' + elm$core$String$fromInt(portion)),
				_List_fromArray(
					[
						A3(
						mdgriffith$elm_ui$Internal$Model$Single,
						mdgriffith$elm_ui$Internal$Style$classes.any + ('.' + (mdgriffith$elm_ui$Internal$Style$classes.row + (' > ' + mdgriffith$elm_ui$Internal$Style$dot(
							'height-fill-' + elm$core$String$fromInt(portion))))),
						'flex-grow',
						elm$core$String$fromInt(portion * 100000))
					]));
		case 'Min':
			var minSize = h.a;
			var len = h.b;
			var cls = 'min-height-' + elm$core$String$fromInt(minSize);
			var style = A3(
				mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'min-height',
				elm$core$String$fromInt(minSize) + 'px');
			var _n1 = mdgriffith$elm_ui$Internal$Model$renderHeight(len);
			var newFlag = _n1.a;
			var newAttrs = _n1.b;
			var newStyle = _n1.c;
			return _Utils_Tuple3(
				A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$heightBetween, newFlag),
				cls + (' ' + newAttrs),
				A2(elm$core$List$cons, style, newStyle));
		default:
			var maxSize = h.a;
			var len = h.b;
			var cls = 'max-height-' + elm$core$String$fromInt(maxSize);
			var style = A3(
				mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'max-height',
				elm$core$String$fromInt(maxSize) + 'px');
			var _n2 = mdgriffith$elm_ui$Internal$Model$renderHeight(len);
			var newFlag = _n2.a;
			var newAttrs = _n2.b;
			var newStyle = _n2.c;
			return _Utils_Tuple3(
				A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$heightBetween, newFlag),
				cls + (' ' + newAttrs),
				A2(elm$core$List$cons, style, newStyle));
	}
};
var mdgriffith$elm_ui$Internal$Model$renderWidth = function (w) {
	switch (w.$) {
		case 'Px':
			var px = w.a;
			return _Utils_Tuple3(
				mdgriffith$elm_ui$Internal$Flag$none,
				mdgriffith$elm_ui$Internal$Style$classes.widthExact + (' width-px-' + elm$core$String$fromInt(px)),
				_List_fromArray(
					[
						A3(
						mdgriffith$elm_ui$Internal$Model$Single,
						'width-px-' + elm$core$String$fromInt(px),
						'width',
						elm$core$String$fromInt(px) + 'px')
					]));
		case 'Content':
			return _Utils_Tuple3(
				A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$widthContent, mdgriffith$elm_ui$Internal$Flag$none),
				mdgriffith$elm_ui$Internal$Style$classes.widthContent,
				_List_Nil);
		case 'Fill':
			var portion = w.a;
			return (portion === 1) ? _Utils_Tuple3(
				A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$widthFill, mdgriffith$elm_ui$Internal$Flag$none),
				mdgriffith$elm_ui$Internal$Style$classes.widthFill,
				_List_Nil) : _Utils_Tuple3(
				A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$widthFill, mdgriffith$elm_ui$Internal$Flag$none),
				mdgriffith$elm_ui$Internal$Style$classes.widthFillPortion + (' width-fill-' + elm$core$String$fromInt(portion)),
				_List_fromArray(
					[
						A3(
						mdgriffith$elm_ui$Internal$Model$Single,
						mdgriffith$elm_ui$Internal$Style$classes.any + ('.' + (mdgriffith$elm_ui$Internal$Style$classes.row + (' > ' + mdgriffith$elm_ui$Internal$Style$dot(
							'width-fill-' + elm$core$String$fromInt(portion))))),
						'flex-grow',
						elm$core$String$fromInt(portion * 100000))
					]));
		case 'Min':
			var minSize = w.a;
			var len = w.b;
			var cls = 'min-width-' + elm$core$String$fromInt(minSize);
			var style = A3(
				mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'min-width',
				elm$core$String$fromInt(minSize) + 'px');
			var _n1 = mdgriffith$elm_ui$Internal$Model$renderWidth(len);
			var newFlag = _n1.a;
			var newAttrs = _n1.b;
			var newStyle = _n1.c;
			return _Utils_Tuple3(
				A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$widthBetween, newFlag),
				cls + (' ' + newAttrs),
				A2(elm$core$List$cons, style, newStyle));
		default:
			var maxSize = w.a;
			var len = w.b;
			var cls = 'max-width-' + elm$core$String$fromInt(maxSize);
			var style = A3(
				mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'max-width',
				elm$core$String$fromInt(maxSize) + 'px');
			var _n2 = mdgriffith$elm_ui$Internal$Model$renderWidth(len);
			var newFlag = _n2.a;
			var newAttrs = _n2.b;
			var newStyle = _n2.c;
			return _Utils_Tuple3(
				A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$widthBetween, newFlag),
				cls + (' ' + newAttrs),
				A2(elm$core$List$cons, style, newStyle));
	}
};
var elm$core$Basics$ge = _Utils_ge;
var mdgriffith$elm_ui$Internal$Flag$borderWidth = mdgriffith$elm_ui$Internal$Flag$flag(27);
var mdgriffith$elm_ui$Internal$Model$skippable = F2(
	function (flag, style) {
		if (_Utils_eq(flag, mdgriffith$elm_ui$Internal$Flag$borderWidth)) {
			if (style.$ === 'Single') {
				var val = style.c;
				switch (val) {
					case '0px':
						return true;
					case '1px':
						return true;
					case '2px':
						return true;
					case '3px':
						return true;
					case '4px':
						return true;
					case '5px':
						return true;
					case '6px':
						return true;
					default:
						return false;
				}
			} else {
				return false;
			}
		} else {
			switch (style.$) {
				case 'FontSize':
					var i = style.a;
					return (i >= 8) && (i <= 32);
				case 'PaddingStyle':
					var name = style.a;
					var t = style.b;
					var r = style.c;
					var b = style.d;
					var l = style.e;
					return _Utils_eq(t, b) && (_Utils_eq(t, r) && (_Utils_eq(t, l) && ((t >= 0) && (t <= 24))));
				default:
					return false;
			}
		}
	});
var mdgriffith$elm_ui$Internal$Model$gatherAttrRecursive = F8(
	function (classes, node, has, transform, styles, attrs, children, elementAttrs) {
		gatherAttrRecursive:
		while (true) {
			if (!elementAttrs.b) {
				var _n1 = mdgriffith$elm_ui$Internal$Model$transformClass(transform);
				if (_n1.$ === 'Nothing') {
					return {
						attributes: A2(
							elm$core$List$cons,
							elm$html$Html$Attributes$class(classes),
							attrs),
						children: children,
						has: has,
						node: node,
						styles: styles
					};
				} else {
					var _class = _n1.a;
					return {
						attributes: A2(
							elm$core$List$cons,
							elm$html$Html$Attributes$class(classes + (' ' + _class)),
							attrs),
						children: children,
						has: has,
						node: node,
						styles: A2(
							elm$core$List$cons,
							mdgriffith$elm_ui$Internal$Model$Transform(transform),
							styles)
					};
				}
			} else {
				var attribute = elementAttrs.a;
				var remaining = elementAttrs.b;
				switch (attribute.$) {
					case 'NoAttribute':
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = has,
							$temp$transform = transform,
							$temp$styles = styles,
							$temp$attrs = attrs,
							$temp$children = children,
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 'Class':
						var flag = attribute.a;
						var exactClassName = attribute.b;
						if (A2(mdgriffith$elm_ui$Internal$Flag$present, flag, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							var $temp$classes = exactClassName + (' ' + classes),
								$temp$node = node,
								$temp$has = A2(mdgriffith$elm_ui$Internal$Flag$add, flag, has),
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						}
					case 'Attr':
						var actualAttribute = attribute.a;
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = has,
							$temp$transform = transform,
							$temp$styles = styles,
							$temp$attrs = A2(elm$core$List$cons, actualAttribute, attrs),
							$temp$children = children,
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 'StyleClass':
						var flag = attribute.a;
						var style = attribute.b;
						if (A2(mdgriffith$elm_ui$Internal$Flag$present, flag, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							if (A2(mdgriffith$elm_ui$Internal$Model$skippable, flag, style)) {
								var $temp$classes = mdgriffith$elm_ui$Internal$Model$getStyleName(style) + (' ' + classes),
									$temp$node = node,
									$temp$has = A2(mdgriffith$elm_ui$Internal$Flag$add, flag, has),
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							} else {
								var $temp$classes = mdgriffith$elm_ui$Internal$Model$getStyleName(style) + (' ' + classes),
									$temp$node = node,
									$temp$has = A2(mdgriffith$elm_ui$Internal$Flag$add, flag, has),
									$temp$transform = transform,
									$temp$styles = A2(elm$core$List$cons, style, styles),
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							}
						}
					case 'TransformComponent':
						var flag = attribute.a;
						var component = attribute.b;
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = A2(mdgriffith$elm_ui$Internal$Flag$add, flag, has),
							$temp$transform = A2(mdgriffith$elm_ui$Internal$Model$composeTransformation, transform, component),
							$temp$styles = styles,
							$temp$attrs = attrs,
							$temp$children = children,
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 'Width':
						var width = attribute.a;
						if (A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$width, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							switch (width.$) {
								case 'Px':
									var px = width.a;
									var $temp$classes = (mdgriffith$elm_ui$Internal$Style$classes.widthExact + (' width-px-' + elm$core$String$fromInt(px))) + (' ' + classes),
										$temp$node = node,
										$temp$has = A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$width, has),
										$temp$transform = transform,
										$temp$styles = A2(
										elm$core$List$cons,
										A3(
											mdgriffith$elm_ui$Internal$Model$Single,
											'width-px-' + elm$core$String$fromInt(px),
											'width',
											elm$core$String$fromInt(px) + 'px'),
										styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 'Content':
									var $temp$classes = classes + (' ' + mdgriffith$elm_ui$Internal$Style$classes.widthContent),
										$temp$node = node,
										$temp$has = A2(
										mdgriffith$elm_ui$Internal$Flag$add,
										mdgriffith$elm_ui$Internal$Flag$widthContent,
										A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$width, has)),
										$temp$transform = transform,
										$temp$styles = styles,
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 'Fill':
									var portion = width.a;
									if (portion === 1) {
										var $temp$classes = classes + (' ' + mdgriffith$elm_ui$Internal$Style$classes.widthFill),
											$temp$node = node,
											$temp$has = A2(
											mdgriffith$elm_ui$Internal$Flag$add,
											mdgriffith$elm_ui$Internal$Flag$widthFill,
											A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$width, has)),
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									} else {
										var $temp$classes = classes + (' ' + (mdgriffith$elm_ui$Internal$Style$classes.widthFillPortion + (' width-fill-' + elm$core$String$fromInt(portion)))),
											$temp$node = node,
											$temp$has = A2(
											mdgriffith$elm_ui$Internal$Flag$add,
											mdgriffith$elm_ui$Internal$Flag$widthFill,
											A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$width, has)),
											$temp$transform = transform,
											$temp$styles = A2(
											elm$core$List$cons,
											A3(
												mdgriffith$elm_ui$Internal$Model$Single,
												mdgriffith$elm_ui$Internal$Style$classes.any + ('.' + (mdgriffith$elm_ui$Internal$Style$classes.row + (' > ' + mdgriffith$elm_ui$Internal$Style$dot(
													'width-fill-' + elm$core$String$fromInt(portion))))),
												'flex-grow',
												elm$core$String$fromInt(portion * 100000)),
											styles),
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									}
								default:
									var _n4 = mdgriffith$elm_ui$Internal$Model$renderWidth(width);
									var addToFlags = _n4.a;
									var newClass = _n4.b;
									var newStyles = _n4.c;
									var $temp$classes = classes + (' ' + newClass),
										$temp$node = node,
										$temp$has = A2(mdgriffith$elm_ui$Internal$Flag$merge, addToFlags, has),
										$temp$transform = transform,
										$temp$styles = _Utils_ap(newStyles, styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
							}
						}
					case 'Height':
						var height = attribute.a;
						if (A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$height, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							switch (height.$) {
								case 'Px':
									var px = height.a;
									var val = elm$core$String$fromInt(px) + 'px';
									var name = 'height-px-' + val;
									var $temp$classes = name + (' ' + classes),
										$temp$node = node,
										$temp$has = A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$height, has),
										$temp$transform = transform,
										$temp$styles = A2(
										elm$core$List$cons,
										A3(mdgriffith$elm_ui$Internal$Model$Single, name, 'height ', val),
										styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 'Content':
									var $temp$classes = mdgriffith$elm_ui$Internal$Style$classes.heightContent + (' ' + classes),
										$temp$node = node,
										$temp$has = A2(
										mdgriffith$elm_ui$Internal$Flag$add,
										mdgriffith$elm_ui$Internal$Flag$heightContent,
										A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$height, has)),
										$temp$transform = transform,
										$temp$styles = styles,
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 'Fill':
									var portion = height.a;
									if (portion === 1) {
										var $temp$classes = mdgriffith$elm_ui$Internal$Style$classes.heightFill + (' ' + classes),
											$temp$node = node,
											$temp$has = A2(
											mdgriffith$elm_ui$Internal$Flag$add,
											mdgriffith$elm_ui$Internal$Flag$heightFill,
											A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$height, has)),
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									} else {
										var $temp$classes = classes + (' ' + (mdgriffith$elm_ui$Internal$Style$classes.heightFillPortion + (' height-fill-' + elm$core$String$fromInt(portion)))),
											$temp$node = node,
											$temp$has = A2(
											mdgriffith$elm_ui$Internal$Flag$add,
											mdgriffith$elm_ui$Internal$Flag$heightFill,
											A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$height, has)),
											$temp$transform = transform,
											$temp$styles = A2(
											elm$core$List$cons,
											A3(
												mdgriffith$elm_ui$Internal$Model$Single,
												mdgriffith$elm_ui$Internal$Style$classes.any + ('.' + (mdgriffith$elm_ui$Internal$Style$classes.column + (' > ' + mdgriffith$elm_ui$Internal$Style$dot(
													'height-fill-' + elm$core$String$fromInt(portion))))),
												'flex-grow',
												elm$core$String$fromInt(portion * 100000)),
											styles),
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									}
								default:
									var _n6 = mdgriffith$elm_ui$Internal$Model$renderHeight(height);
									var addToFlags = _n6.a;
									var newClass = _n6.b;
									var newStyles = _n6.c;
									var $temp$classes = classes + (' ' + newClass),
										$temp$node = node,
										$temp$has = A2(mdgriffith$elm_ui$Internal$Flag$merge, addToFlags, has),
										$temp$transform = transform,
										$temp$styles = _Utils_ap(newStyles, styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
							}
						}
					case 'Describe':
						var description = attribute.a;
						switch (description.$) {
							case 'Main':
								var $temp$classes = classes,
									$temp$node = A2(mdgriffith$elm_ui$Internal$Model$addNodeName, 'main', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 'Navigation':
								var $temp$classes = classes,
									$temp$node = A2(mdgriffith$elm_ui$Internal$Model$addNodeName, 'nav', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 'ContentInfo':
								var $temp$classes = classes,
									$temp$node = A2(mdgriffith$elm_ui$Internal$Model$addNodeName, 'footer', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 'Complementary':
								var $temp$classes = classes,
									$temp$node = A2(mdgriffith$elm_ui$Internal$Model$addNodeName, 'aside', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 'Heading':
								var i = description.a;
								if (i <= 1) {
									var $temp$classes = classes,
										$temp$node = A2(mdgriffith$elm_ui$Internal$Model$addNodeName, 'h1', node),
										$temp$has = has,
										$temp$transform = transform,
										$temp$styles = styles,
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								} else {
									if (i < 7) {
										var $temp$classes = classes,
											$temp$node = A2(
											mdgriffith$elm_ui$Internal$Model$addNodeName,
											'h' + elm$core$String$fromInt(i),
											node),
											$temp$has = has,
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									} else {
										var $temp$classes = classes,
											$temp$node = A2(mdgriffith$elm_ui$Internal$Model$addNodeName, 'h6', node),
											$temp$has = has,
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									}
								}
							case 'Paragraph':
								var newNode = function () {
									switch (node.$) {
										case 'Generic':
											return mdgriffith$elm_ui$Internal$Model$NodeName('p');
										case 'NodeName':
											var name = node.a;
											return mdgriffith$elm_ui$Internal$Model$NodeName(name);
										default:
											var x = node.a;
											var y = node.b;
											return A2(mdgriffith$elm_ui$Internal$Model$Embedded, x, y);
									}
								}();
								var $temp$classes = classes,
									$temp$node = newNode,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 'Button':
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									elm$core$List$cons,
									A2(elm$virtual_dom$VirtualDom$attribute, 'role', 'button'),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 'Label':
								var label = description.a;
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									elm$core$List$cons,
									A2(elm$virtual_dom$VirtualDom$attribute, 'aria-label', label),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 'LivePolite':
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									elm$core$List$cons,
									A2(elm$virtual_dom$VirtualDom$attribute, 'aria-live', 'polite'),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							default:
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									elm$core$List$cons,
									A2(elm$virtual_dom$VirtualDom$attribute, 'aria-live', 'assertive'),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
						}
					case 'Nearby':
						var location = attribute.a;
						var elem = attribute.b;
						var newStyles = function () {
							switch (elem.$) {
								case 'Empty':
									return styles;
								case 'Text':
									var str = elem.a;
									return styles;
								case 'Unstyled':
									var html = elem.a;
									return styles;
								default:
									var styled = elem.a;
									return _Utils_ap(styles, styled.styles);
							}
						}();
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = has,
							$temp$transform = transform,
							$temp$styles = newStyles,
							$temp$attrs = attrs,
							$temp$children = A3(mdgriffith$elm_ui$Internal$Model$addNearbyElement, location, elem, children),
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 'AlignX':
						var x = attribute.a;
						if (A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$xAlign, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							var $temp$classes = mdgriffith$elm_ui$Internal$Model$alignXName(x) + (' ' + classes),
								$temp$node = node,
								$temp$has = function (flags) {
								switch (x.$) {
									case 'CenterX':
										return A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$centerX, flags);
									case 'Right':
										return A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$alignRight, flags);
									default:
										return flags;
								}
							}(
								A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$xAlign, has)),
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						}
					default:
						var y = attribute.a;
						if (A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$yAlign, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							var $temp$classes = mdgriffith$elm_ui$Internal$Model$alignYName(y) + (' ' + classes),
								$temp$node = node,
								$temp$has = function (flags) {
								switch (y.$) {
									case 'CenterY':
										return A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$centerY, flags);
									case 'Bottom':
										return A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$alignBottom, flags);
									default:
										return flags;
								}
							}(
								A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$yAlign, has)),
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						}
				}
			}
		}
	});
var mdgriffith$elm_ui$Internal$Model$Untransformed = {$: 'Untransformed'};
var mdgriffith$elm_ui$Internal$Model$untransformed = mdgriffith$elm_ui$Internal$Model$Untransformed;
var mdgriffith$elm_ui$Internal$Model$element = F4(
	function (context, node, attributes, children) {
		return A3(
			mdgriffith$elm_ui$Internal$Model$createElement,
			context,
			children,
			A8(
				mdgriffith$elm_ui$Internal$Model$gatherAttrRecursive,
				mdgriffith$elm_ui$Internal$Model$contextClasses(context),
				node,
				mdgriffith$elm_ui$Internal$Flag$none,
				mdgriffith$elm_ui$Internal$Model$untransformed,
				_List_Nil,
				_List_Nil,
				mdgriffith$elm_ui$Internal$Model$NoNearbyChildren,
				elm$core$List$reverse(attributes)));
	});
var mdgriffith$elm_ui$Internal$Model$Attr = function (a) {
	return {$: 'Attr', a: a};
};
var mdgriffith$elm_ui$Internal$Model$htmlClass = function (cls) {
	return mdgriffith$elm_ui$Internal$Model$Attr(
		elm$html$Html$Attributes$class(cls));
};
var mdgriffith$elm_ui$Element$column = F2(
	function (attrs, children) {
		return A4(
			mdgriffith$elm_ui$Internal$Model$element,
			mdgriffith$elm_ui$Internal$Model$asColumn,
			mdgriffith$elm_ui$Internal$Model$div,
			A2(
				elm$core$List$cons,
				mdgriffith$elm_ui$Internal$Model$htmlClass(mdgriffith$elm_ui$Internal$Style$classes.contentTop + (' ' + mdgriffith$elm_ui$Internal$Style$classes.contentLeft)),
				A2(
					elm$core$List$cons,
					mdgriffith$elm_ui$Element$height(mdgriffith$elm_ui$Element$shrink),
					A2(
						elm$core$List$cons,
						mdgriffith$elm_ui$Element$width(mdgriffith$elm_ui$Element$shrink),
						attrs))),
			mdgriffith$elm_ui$Internal$Model$Unkeyed(children));
	});
var mdgriffith$elm_ui$Internal$Flag$padding = mdgriffith$elm_ui$Internal$Flag$flag(2);
var mdgriffith$elm_ui$Internal$Model$PaddingStyle = F5(
	function (a, b, c, d, e) {
		return {$: 'PaddingStyle', a: a, b: b, c: c, d: d, e: e};
	});
var mdgriffith$elm_ui$Element$paddingXY = F2(
	function (x, y) {
		return _Utils_eq(x, y) ? A2(
			mdgriffith$elm_ui$Internal$Model$StyleClass,
			mdgriffith$elm_ui$Internal$Flag$padding,
			A5(
				mdgriffith$elm_ui$Internal$Model$PaddingStyle,
				'p-' + elm$core$String$fromInt(x),
				x,
				x,
				x,
				x)) : A2(
			mdgriffith$elm_ui$Internal$Model$StyleClass,
			mdgriffith$elm_ui$Internal$Flag$padding,
			A5(
				mdgriffith$elm_ui$Internal$Model$PaddingStyle,
				'p-' + (elm$core$String$fromInt(x) + ('-' + elm$core$String$fromInt(y))),
				y,
				x,
				y,
				x));
	});
var mdgriffith$elm_ui$Internal$Model$Text = function (a) {
	return {$: 'Text', a: a};
};
var mdgriffith$elm_ui$Element$text = function (content) {
	return mdgriffith$elm_ui$Internal$Model$Text(content);
};
var mdgriffith$elm_ui$Internal$Flag$bgColor = mdgriffith$elm_ui$Internal$Flag$flag(8);
var mdgriffith$elm_ui$Internal$Model$Colored = F3(
	function (a, b, c) {
		return {$: 'Colored', a: a, b: b, c: c};
	});
var mdgriffith$elm_ui$Internal$Model$formatColorClass = function (_n0) {
	var red = _n0.a;
	var green = _n0.b;
	var blue = _n0.c;
	var alpha = _n0.d;
	return mdgriffith$elm_ui$Internal$Model$floatClass(red) + ('-' + (mdgriffith$elm_ui$Internal$Model$floatClass(green) + ('-' + (mdgriffith$elm_ui$Internal$Model$floatClass(blue) + ('-' + mdgriffith$elm_ui$Internal$Model$floatClass(alpha))))));
};
var mdgriffith$elm_ui$Element$Background$color = function (clr) {
	return A2(
		mdgriffith$elm_ui$Internal$Model$StyleClass,
		mdgriffith$elm_ui$Internal$Flag$bgColor,
		A3(
			mdgriffith$elm_ui$Internal$Model$Colored,
			'bg-' + mdgriffith$elm_ui$Internal$Model$formatColorClass(clr),
			'background-color',
			clr));
};
var mdgriffith$elm_ui$Internal$Flag$borderRound = mdgriffith$elm_ui$Internal$Flag$flag(17);
var mdgriffith$elm_ui$Element$Border$rounded = function (radius) {
	return A2(
		mdgriffith$elm_ui$Internal$Model$StyleClass,
		mdgriffith$elm_ui$Internal$Flag$borderRound,
		A3(
			mdgriffith$elm_ui$Internal$Model$Single,
			'br-' + elm$core$String$fromInt(radius),
			'border-radius',
			elm$core$String$fromInt(radius) + 'px'));
};
var mdgriffith$elm_ui$Internal$Flag$fontSize = mdgriffith$elm_ui$Internal$Flag$flag(4);
var mdgriffith$elm_ui$Internal$Model$FontSize = function (a) {
	return {$: 'FontSize', a: a};
};
var mdgriffith$elm_ui$Element$Font$size = function (i) {
	return A2(
		mdgriffith$elm_ui$Internal$Model$StyleClass,
		mdgriffith$elm_ui$Internal$Flag$fontSize,
		mdgriffith$elm_ui$Internal$Model$FontSize(i));
};
var author$project$Guide$Document$viewCodeBlock = F2(
	function (screenType, code) {
		return A2(
			mdgriffith$elm_ui$Element$column,
			_List_fromArray(
				[
					mdgriffith$elm_ui$Element$Border$rounded(5),
					A2(mdgriffith$elm_ui$Element$paddingXY, 10, 4),
					author$project$Guide$Document$sourceCodePro,
					mdgriffith$elm_ui$Element$Background$color(author$project$Guide$Document$lightGrey),
					mdgriffith$elm_ui$Element$Font$size(
					author$project$Guide$Document$fontSizes(screenType).bodyCode)
				]),
			A2(
				elm$core$List$map,
				mdgriffith$elm_ui$Element$text,
				elm$core$String$lines(
					elm$core$String$trim(code))));
	});
var mdgriffith$elm_ui$Element$el = F2(
	function (attrs, child) {
		return A4(
			mdgriffith$elm_ui$Internal$Model$element,
			mdgriffith$elm_ui$Internal$Model$asEl,
			mdgriffith$elm_ui$Internal$Model$div,
			A2(
				elm$core$List$cons,
				mdgriffith$elm_ui$Element$width(mdgriffith$elm_ui$Element$shrink),
				A2(
					elm$core$List$cons,
					mdgriffith$elm_ui$Element$height(mdgriffith$elm_ui$Element$shrink),
					attrs)),
			mdgriffith$elm_ui$Internal$Model$Unkeyed(
				_List_fromArray(
					[child])));
	});
var author$project$Guide$Document$inlineCodeElement = F2(
	function (fontSize, chunk) {
		if (chunk.$ === 'Characters') {
			var characters = chunk.a;
			return mdgriffith$elm_ui$Element$text(characters);
		} else {
			var spaces = chunk.a;
			return A2(
				mdgriffith$elm_ui$Element$el,
				_List_fromArray(
					[
						mdgriffith$elm_ui$Element$Font$size(
						elm$core$Basics$round((2 / 3) * fontSize))
					]),
				mdgriffith$elm_ui$Element$text(spaces));
		}
	});
var mdgriffith$elm_ui$Internal$Model$AsRow = {$: 'AsRow'};
var mdgriffith$elm_ui$Internal$Model$asRow = mdgriffith$elm_ui$Internal$Model$AsRow;
var mdgriffith$elm_ui$Element$row = F2(
	function (attrs, children) {
		return A4(
			mdgriffith$elm_ui$Internal$Model$element,
			mdgriffith$elm_ui$Internal$Model$asRow,
			mdgriffith$elm_ui$Internal$Model$div,
			A2(
				elm$core$List$cons,
				mdgriffith$elm_ui$Internal$Model$htmlClass(mdgriffith$elm_ui$Internal$Style$classes.contentLeft + (' ' + mdgriffith$elm_ui$Internal$Style$classes.contentCenterY)),
				A2(
					elm$core$List$cons,
					mdgriffith$elm_ui$Element$width(mdgriffith$elm_ui$Element$shrink),
					A2(
						elm$core$List$cons,
						mdgriffith$elm_ui$Element$height(mdgriffith$elm_ui$Element$shrink),
						attrs))),
			mdgriffith$elm_ui$Internal$Model$Unkeyed(children));
	});
var mdgriffith$elm_ui$Internal$Flag$fontWeight = mdgriffith$elm_ui$Internal$Flag$flag(13);
var mdgriffith$elm_ui$Internal$Model$Class = F2(
	function (a, b) {
		return {$: 'Class', a: a, b: b};
	});
var mdgriffith$elm_ui$Element$Font$bold = A2(mdgriffith$elm_ui$Internal$Model$Class, mdgriffith$elm_ui$Internal$Flag$fontWeight, mdgriffith$elm_ui$Internal$Style$classes.bold);
var mdgriffith$elm_ui$Element$Font$italic = mdgriffith$elm_ui$Internal$Model$htmlClass(mdgriffith$elm_ui$Internal$Style$classes.italic);
var author$project$Guide$Document$renderTextFragment = F3(
	function (screenType, context, fragment) {
		switch (fragment.$) {
			case 'Plain':
				var string = fragment.a;
				return mdgriffith$elm_ui$Element$text(string);
			case 'Italic':
				var string = fragment.a;
				return A2(
					mdgriffith$elm_ui$Element$el,
					_List_fromArray(
						[mdgriffith$elm_ui$Element$Font$italic]),
					mdgriffith$elm_ui$Element$text(string));
			case 'Bold':
				var string = fragment.a;
				return A2(
					mdgriffith$elm_ui$Element$el,
					_List_fromArray(
						[mdgriffith$elm_ui$Element$Font$bold]),
					mdgriffith$elm_ui$Element$text(string));
			default:
				var chunks = fragment.a;
				var fontSize = function () {
					switch (context.$) {
						case 'TitleContext':
							return author$project$Guide$Document$fontSizes(screenType).titleCode;
						case 'SectionContext':
							return author$project$Guide$Document$fontSizes(screenType).sectionCode;
						case 'SubsectionContext':
							return author$project$Guide$Document$fontSizes(screenType).subsectionCode;
						case 'ParagraphContext':
							return author$project$Guide$Document$fontSizes(screenType).bodyCode;
						default:
							return author$project$Guide$Document$fontSizes(screenType).bodyCode;
					}
				}();
				var backgroundAttributes = _Utils_eq(context, author$project$Guide$Document$ParagraphContext) ? _List_fromArray(
					[
						A2(mdgriffith$elm_ui$Element$paddingXY, 4, 2),
						mdgriffith$elm_ui$Element$Border$rounded(3),
						mdgriffith$elm_ui$Element$Background$color(author$project$Guide$Document$lightGrey)
					]) : _List_Nil;
				return A2(
					mdgriffith$elm_ui$Element$row,
					A2(
						elm$core$List$cons,
						author$project$Guide$Document$sourceCodePro,
						A2(
							elm$core$List$cons,
							mdgriffith$elm_ui$Element$Font$size(fontSize),
							backgroundAttributes)),
					A2(
						elm$core$List$map,
						author$project$Guide$Document$inlineCodeElement(fontSize),
						chunks));
		}
	});
var author$project$Guide$Document$renderText = F3(
	function (screenType, context, fragments) {
		return A2(
			elm$core$List$map,
			A2(author$project$Guide$Document$renderTextFragment, screenType, context),
			fragments);
	});
var mdgriffith$elm_ui$Internal$Model$Fill = function (a) {
	return {$: 'Fill', a: a};
};
var mdgriffith$elm_ui$Element$fill = mdgriffith$elm_ui$Internal$Model$Fill(1);
var mdgriffith$elm_ui$Internal$Flag$spacing = mdgriffith$elm_ui$Internal$Flag$flag(3);
var mdgriffith$elm_ui$Internal$Model$SpacingStyle = F3(
	function (a, b, c) {
		return {$: 'SpacingStyle', a: a, b: b, c: c};
	});
var mdgriffith$elm_ui$Internal$Model$spacingName = F2(
	function (x, y) {
		return 'spacing-' + (elm$core$String$fromInt(x) + ('-' + elm$core$String$fromInt(y)));
	});
var mdgriffith$elm_ui$Element$spacing = function (x) {
	return A2(
		mdgriffith$elm_ui$Internal$Model$StyleClass,
		mdgriffith$elm_ui$Internal$Flag$spacing,
		A3(
			mdgriffith$elm_ui$Internal$Model$SpacingStyle,
			A2(mdgriffith$elm_ui$Internal$Model$spacingName, x, x),
			x,
			x));
};
var mdgriffith$elm_ui$Internal$Model$Describe = function (a) {
	return {$: 'Describe', a: a};
};
var mdgriffith$elm_ui$Internal$Model$Paragraph = {$: 'Paragraph'};
var mdgriffith$elm_ui$Element$paragraph = F2(
	function (attrs, children) {
		return A4(
			mdgriffith$elm_ui$Internal$Model$element,
			mdgriffith$elm_ui$Internal$Model$asParagraph,
			mdgriffith$elm_ui$Internal$Model$div,
			A2(
				elm$core$List$cons,
				mdgriffith$elm_ui$Internal$Model$Describe(mdgriffith$elm_ui$Internal$Model$Paragraph),
				A2(
					elm$core$List$cons,
					mdgriffith$elm_ui$Element$width(mdgriffith$elm_ui$Element$fill),
					A2(
						elm$core$List$cons,
						mdgriffith$elm_ui$Element$spacing(5),
						attrs))),
			mdgriffith$elm_ui$Internal$Model$Unkeyed(children));
	});
var author$project$Guide$Document$viewParagraph = F2(
	function (screenType, textFragments) {
		return A2(
			mdgriffith$elm_ui$Element$paragraph,
			_List_Nil,
			A3(author$project$Guide$Document$renderText, screenType, author$project$Guide$Document$ParagraphContext, textFragments));
	});
var mdgriffith$elm_ui$Internal$Model$SansSerif = {$: 'SansSerif'};
var mdgriffith$elm_ui$Element$Font$sansSerif = mdgriffith$elm_ui$Internal$Model$SansSerif;
var author$project$Guide$Document$alegreyaSans = mdgriffith$elm_ui$Element$Font$family(
	_List_fromArray(
		[
			mdgriffith$elm_ui$Element$Font$typeface('Alegreya Sans'),
			mdgriffith$elm_ui$Element$Font$sansSerif
		]));
var mdgriffith$elm_ui$Internal$Model$paddingName = F4(
	function (top, right, bottom, left) {
		return 'pad-' + (elm$core$String$fromInt(top) + ('-' + (elm$core$String$fromInt(right) + ('-' + (elm$core$String$fromInt(bottom) + ('-' + elm$core$String$fromInt(left)))))));
	});
var mdgriffith$elm_ui$Element$paddingEach = function (_n0) {
	var top = _n0.top;
	var right = _n0.right;
	var bottom = _n0.bottom;
	var left = _n0.left;
	return (_Utils_eq(top, right) && (_Utils_eq(top, bottom) && _Utils_eq(top, left))) ? A2(
		mdgriffith$elm_ui$Internal$Model$StyleClass,
		mdgriffith$elm_ui$Internal$Flag$padding,
		A5(
			mdgriffith$elm_ui$Internal$Model$PaddingStyle,
			'p-' + elm$core$String$fromInt(top),
			top,
			top,
			top,
			top)) : A2(
		mdgriffith$elm_ui$Internal$Model$StyleClass,
		mdgriffith$elm_ui$Internal$Flag$padding,
		A5(
			mdgriffith$elm_ui$Internal$Model$PaddingStyle,
			A4(mdgriffith$elm_ui$Internal$Model$paddingName, top, right, bottom, left),
			top,
			right,
			bottom,
			left));
};
var mdgriffith$elm_ui$Element$Font$extraBold = A2(mdgriffith$elm_ui$Internal$Model$Class, mdgriffith$elm_ui$Internal$Flag$fontWeight, mdgriffith$elm_ui$Internal$Style$classes.textExtraBold);
var elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var mdgriffith$elm_ui$Internal$Model$Heading = function (a) {
	return {$: 'Heading', a: a};
};
var mdgriffith$elm_ui$Element$Region$heading = A2(elm$core$Basics$composeL, mdgriffith$elm_ui$Internal$Model$Describe, mdgriffith$elm_ui$Internal$Model$Heading);
var author$project$Guide$Document$viewSection = F2(
	function (screenType, textFragments) {
		return A2(
			mdgriffith$elm_ui$Element$paragraph,
			_List_fromArray(
				[
					mdgriffith$elm_ui$Element$Region$heading(2),
					author$project$Guide$Document$alegreyaSans,
					mdgriffith$elm_ui$Element$Font$extraBold,
					mdgriffith$elm_ui$Element$Font$size(
					author$project$Guide$Document$fontSizes(screenType).section),
					mdgriffith$elm_ui$Element$paddingEach(
					{bottom: 0, left: 0, right: 0, top: 12})
				]),
			A3(author$project$Guide$Document$renderText, screenType, author$project$Guide$Document$SectionContext, textFragments));
	});
var author$project$Guide$Document$viewSubsection = F2(
	function (screenType, textFragments) {
		return A2(
			mdgriffith$elm_ui$Element$paragraph,
			_List_fromArray(
				[
					mdgriffith$elm_ui$Element$Region$heading(3),
					author$project$Guide$Document$alegreyaSans,
					mdgriffith$elm_ui$Element$Font$extraBold,
					mdgriffith$elm_ui$Element$Font$size(
					author$project$Guide$Document$fontSizes(screenType).subsection),
					mdgriffith$elm_ui$Element$paddingEach(
					{bottom: 0, left: 0, right: 0, top: 6})
				]),
			A3(author$project$Guide$Document$renderText, screenType, author$project$Guide$Document$SubsectionContext, textFragments));
	});
var mdgriffith$elm_ui$Internal$Flag$borderColor = mdgriffith$elm_ui$Internal$Flag$flag(28);
var mdgriffith$elm_ui$Element$Border$color = function (clr) {
	return A2(
		mdgriffith$elm_ui$Internal$Model$StyleClass,
		mdgriffith$elm_ui$Internal$Flag$borderColor,
		A3(
			mdgriffith$elm_ui$Internal$Model$Colored,
			'bc-' + mdgriffith$elm_ui$Internal$Model$formatColorClass(clr),
			'border-color',
			clr));
};
var mdgriffith$elm_ui$Internal$Model$BorderWidth = F5(
	function (a, b, c, d, e) {
		return {$: 'BorderWidth', a: a, b: b, c: c, d: d, e: e};
	});
var mdgriffith$elm_ui$Element$Border$width = function (v) {
	return A2(
		mdgriffith$elm_ui$Internal$Model$StyleClass,
		mdgriffith$elm_ui$Internal$Flag$borderWidth,
		A5(
			mdgriffith$elm_ui$Internal$Model$BorderWidth,
			'b-' + elm$core$String$fromInt(v),
			v,
			v,
			v,
			v));
};
var mdgriffith$elm_ui$Element$Border$widthXY = F2(
	function (x, y) {
		return A2(
			mdgriffith$elm_ui$Internal$Model$StyleClass,
			mdgriffith$elm_ui$Internal$Flag$borderWidth,
			A5(
				mdgriffith$elm_ui$Internal$Model$BorderWidth,
				'b-' + (elm$core$String$fromInt(x) + ('-' + elm$core$String$fromInt(y))),
				y,
				x,
				y,
				x));
	});
var mdgriffith$elm_ui$Element$Border$widthEach = function (_n0) {
	var bottom = _n0.bottom;
	var top = _n0.top;
	var left = _n0.left;
	var right = _n0.right;
	return (_Utils_eq(top, bottom) && _Utils_eq(left, right)) ? (_Utils_eq(top, right) ? mdgriffith$elm_ui$Element$Border$width(top) : A2(mdgriffith$elm_ui$Element$Border$widthXY, left, top)) : A2(
		mdgriffith$elm_ui$Internal$Model$StyleClass,
		mdgriffith$elm_ui$Internal$Flag$borderWidth,
		A5(
			mdgriffith$elm_ui$Internal$Model$BorderWidth,
			'b-' + (elm$core$String$fromInt(top) + ('-' + (elm$core$String$fromInt(right) + ('-' + (elm$core$String$fromInt(bottom) + ('-' + elm$core$String$fromInt(left))))))),
			top,
			right,
			bottom,
			left));
};
var author$project$Guide$Document$viewTitle = F2(
	function (screenType, textFragments) {
		return A2(
			mdgriffith$elm_ui$Element$el,
			_List_fromArray(
				[
					mdgriffith$elm_ui$Element$width(mdgriffith$elm_ui$Element$fill),
					mdgriffith$elm_ui$Element$paddingEach(
					{bottom: 8, left: 0, right: 0, top: 8})
				]),
			A2(
				mdgriffith$elm_ui$Element$paragraph,
				_List_fromArray(
					[
						mdgriffith$elm_ui$Element$Region$heading(1),
						author$project$Guide$Document$alegreyaSans,
						mdgriffith$elm_ui$Element$Font$extraBold,
						mdgriffith$elm_ui$Element$Font$size(
						author$project$Guide$Document$fontSizes(screenType).title),
						mdgriffith$elm_ui$Element$Border$widthEach(
						{bottom: 1, left: 0, right: 0, top: 0}),
						mdgriffith$elm_ui$Element$Border$color(author$project$Guide$Document$lightGrey),
						mdgriffith$elm_ui$Element$width(mdgriffith$elm_ui$Element$fill)
					]),
				A3(author$project$Guide$Document$renderText, screenType, author$project$Guide$Document$TitleContext, textFragments)));
	});
var author$project$Guide$Document$compileBullets = F4(
	function (screenType, bullets, widgetId, accumulated) {
		compileBullets:
		while (true) {
			if (bullets.b) {
				var chunks = bullets.a;
				var rest = bullets.b;
				var _n4 = A4(author$project$Guide$Document$compileHelp, screenType, chunks, widgetId, _List_Nil);
				var compiledChunks = _n4.a;
				var updatedId = _n4.b;
				var $temp$screenType = screenType,
					$temp$bullets = rest,
					$temp$widgetId = updatedId,
					$temp$accumulated = A2(elm$core$List$cons, compiledChunks, accumulated);
				screenType = $temp$screenType;
				bullets = $temp$bullets;
				widgetId = $temp$widgetId;
				accumulated = $temp$accumulated;
				continue compileBullets;
			} else {
				return _Utils_Tuple2(
					elm$core$List$reverse(accumulated),
					widgetId);
			}
		}
	});
var author$project$Guide$Document$compileHelp = F4(
	function (screenType, chunks, widgetId, accumulated) {
		compileHelp:
		while (true) {
			if (chunks.b) {
				var first = chunks.a;
				var rest = chunks.b;
				var prepend = function (compiledChunk) {
					return A4(
						author$project$Guide$Document$compileHelp,
						screenType,
						rest,
						widgetId,
						A2(elm$core$List$cons, compiledChunk, accumulated));
				};
				switch (first.$) {
					case 'Title':
						var textFragments = first.a;
						return prepend(
							A2(
								author$project$Guide$Document$Static,
								author$project$Guide$Document$TitleContext,
								A2(author$project$Guide$Document$viewTitle, screenType, textFragments)));
					case 'Section':
						var textFragments = first.a;
						return prepend(
							A2(
								author$project$Guide$Document$Static,
								author$project$Guide$Document$SectionContext,
								A2(author$project$Guide$Document$viewSection, screenType, textFragments)));
					case 'Subsection':
						var textFragments = first.a;
						return prepend(
							A2(
								author$project$Guide$Document$Static,
								author$project$Guide$Document$SubsectionContext,
								A2(author$project$Guide$Document$viewSubsection, screenType, textFragments)));
					case 'Paragraph':
						var textFragments = first.a;
						return prepend(
							A2(
								author$project$Guide$Document$Static,
								author$project$Guide$Document$ParagraphContext,
								A2(author$project$Guide$Document$viewParagraph, screenType, textFragments)));
					case 'CustomBlock':
						var widget = first.a;
						var $temp$screenType = screenType,
							$temp$chunks = rest,
							$temp$widgetId = widgetId + 1,
							$temp$accumulated = A2(
							elm$core$List$cons,
							A2(author$project$Guide$Document$Interactive, widgetId, widget),
							accumulated);
						screenType = $temp$screenType;
						chunks = $temp$chunks;
						widgetId = $temp$widgetId;
						accumulated = $temp$accumulated;
						continue compileHelp;
					case 'CodeBlock':
						var code = first.a;
						return prepend(
							A2(
								author$project$Guide$Document$Static,
								author$project$Guide$Document$CodeBlockContext,
								A2(author$project$Guide$Document$viewCodeBlock, screenType, code)));
					default:
						var bullets = first.a;
						var _n2 = A4(author$project$Guide$Document$compileBullets, screenType, bullets, widgetId, _List_Nil);
						var compiledBullets = _n2.a;
						var updatedId = _n2.b;
						var $temp$screenType = screenType,
							$temp$chunks = rest,
							$temp$widgetId = updatedId,
							$temp$accumulated = A2(
							elm$core$List$cons,
							author$project$Guide$Document$CompiledBullets(compiledBullets),
							accumulated);
						screenType = $temp$screenType;
						chunks = $temp$chunks;
						widgetId = $temp$widgetId;
						accumulated = $temp$accumulated;
						continue compileHelp;
				}
			} else {
				return _Utils_Tuple2(
					elm$core$List$reverse(accumulated),
					widgetId);
			}
		}
	});
var author$project$Guide$Document$compile = F2(
	function (screenType, chunks) {
		return A4(author$project$Guide$Document$compileHelp, screenType, chunks, 0, _List_Nil).a;
	});
var author$project$Guide$Document$Bullets = function (a) {
	return {$: 'Bullets', a: a};
};
var author$project$Guide$Document$CodeBlock = function (a) {
	return {$: 'CodeBlock', a: a};
};
var author$project$Guide$Document$CustomBlock = function (a) {
	return {$: 'CustomBlock', a: a};
};
var author$project$Guide$Document$Paragraph = function (a) {
	return {$: 'Paragraph', a: a};
};
var author$project$Guide$Document$Section = function (a) {
	return {$: 'Section', a: a};
};
var author$project$Guide$Document$Subsection = function (a) {
	return {$: 'Subsection', a: a};
};
var author$project$Guide$Document$Bold = function (a) {
	return {$: 'Bold', a: a};
};
var author$project$Guide$Document$InlineCode = function (a) {
	return {$: 'InlineCode', a: a};
};
var author$project$Guide$Document$Italic = function (a) {
	return {$: 'Italic', a: a};
};
var author$project$Guide$Document$Plain = function (a) {
	return {$: 'Plain', a: a};
};
var elm$regex$Regex$Match = F4(
	function (match, index, number, submatches) {
		return {index: index, match: match, number: number, submatches: submatches};
	});
var elm$regex$Regex$fromStringWith = _Regex_fromStringWith;
var elm$regex$Regex$fromString = function (string) {
	return A2(
		elm$regex$Regex$fromStringWith,
		{caseInsensitive: false, multiline: false},
		string);
};
var elm$regex$Regex$never = _Regex_never;
var author$project$Guide$Document$inlineCodeRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('(\\s+)|(\\S+)'));
var author$project$Guide$Document$Characters = function (a) {
	return {$: 'Characters', a: a};
};
var author$project$Guide$Document$Spaces = function (a) {
	return {$: 'Spaces', a: a};
};
var elm$core$String$length = _String_length;
var elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3(elm$core$String$repeatHelp, n, chunk, '');
	});
var author$project$Guide$Document$toInlineCodeChunk = function (match) {
	var _n0 = match.submatches;
	_n0$2:
	while (true) {
		if (_n0.b) {
			if (_n0.a.$ === 'Just') {
				if ((_n0.b.b && (_n0.b.a.$ === 'Nothing')) && (!_n0.b.b.b)) {
					var spaces = _n0.a.a;
					var _n1 = _n0.b;
					var _n2 = _n1.a;
					return author$project$Guide$Document$Spaces(
						A2(
							elm$core$String$repeat,
							elm$core$String$length(spaces),
							'\u00a0'));
				} else {
					break _n0$2;
				}
			} else {
				if ((_n0.b.b && (_n0.b.a.$ === 'Just')) && (!_n0.b.b.b)) {
					var _n3 = _n0.a;
					var _n4 = _n0.b;
					var characters = _n4.a.a;
					return author$project$Guide$Document$Characters(characters);
				} else {
					break _n0$2;
				}
			}
		} else {
			break _n0$2;
		}
	}
	return author$project$Guide$Document$Spaces('');
};
var elm$regex$Regex$find = _Regex_findAtMost(_Regex_infinity);
var author$project$Guide$Document$toInlineCodeChunks = function (string) {
	var matches = A2(elm$regex$Regex$find, author$project$Guide$Document$inlineCodeRegex, string);
	return A2(elm$core$List$map, author$project$Guide$Document$toInlineCodeChunk, matches);
};
var author$project$Guide$Document$parseText = F2(
	function (inlines, accumulated) {
		if (inlines.b) {
			var first = inlines.a;
			var rest = inlines.b;
			var prepend = function (text) {
				return A2(
					author$project$Guide$Document$parseText,
					rest,
					A2(elm$core$List$cons, text, accumulated));
			};
			_n1$8:
			while (true) {
				switch (first.$) {
					case 'Text':
						var string = first.a;
						return prepend(
							author$project$Guide$Document$Plain(string));
					case 'HardLineBreak':
						return elm$core$Result$Err('Hard line breaks not yet supported');
					case 'CodeInline':
						var string = first.a;
						return prepend(
							author$project$Guide$Document$InlineCode(
								author$project$Guide$Document$toInlineCodeChunks(string)));
					case 'Link':
						return elm$core$Result$Err('Links not yet supported');
					case 'Image':
						return elm$core$Result$Err('Inline images not yet supported');
					case 'HtmlInline':
						return elm$core$Result$Err('Inline custom elements not yet supported');
					case 'Emphasis':
						if ((first.b.b && (first.b.a.$ === 'Text')) && (!first.b.b.b)) {
							switch (first.a) {
								case 1:
									var _n2 = first.b;
									var string = _n2.a.a;
									return prepend(
										author$project$Guide$Document$Italic(string));
								case 2:
									var _n3 = first.b;
									var string = _n3.a.a;
									return prepend(
										author$project$Guide$Document$Bold(string));
								default:
									break _n1$8;
							}
						} else {
							break _n1$8;
						}
					default:
						return elm$core$Result$Err('Internal error - should not be possible to have a Custom inline');
				}
			}
			return elm$core$Result$Err('Only _italic plain text_ and **bold plain text** are currently supported');
		} else {
			return elm$core$Result$Ok(
				elm$core$List$reverse(accumulated));
		}
	});
var elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var elm$core$Debug$toString = _Debug_toString;
var elm$core$Result$andThen = F2(
	function (callback, result) {
		if (result.$ === 'Ok') {
			var value = result.a;
			return callback(value);
		} else {
			var msg = result.a;
			return elm$core$Result$Err(msg);
		}
	});
var elm$core$Result$map = F2(
	function (func, ra) {
		if (ra.$ === 'Ok') {
			var a = ra.a;
			return elm$core$Result$Ok(
				func(a));
		} else {
			var e = ra.a;
			return elm$core$Result$Err(e);
		}
	});
var elm$core$Result$map2 = F3(
	function (func, ra, rb) {
		if (ra.$ === 'Err') {
			var x = ra.a;
			return elm$core$Result$Err(x);
		} else {
			var a = ra.a;
			if (rb.$ === 'Err') {
				var x = rb.a;
				return elm$core$Result$Err(x);
			} else {
				var b = rb.a;
				return elm$core$Result$Ok(
					A2(func, a, b));
			}
		}
	});
var elm_community$result_extra$Result$Extra$combine = A2(
	elm$core$List$foldr,
	elm$core$Result$map2(elm$core$List$cons),
	elm$core$Result$Ok(_List_Nil));
var author$project$Guide$Document$parseChunks = F3(
	function (config, blocks, accumulated) {
		parseChunks:
		while (true) {
			if (blocks.b) {
				var first = blocks.a;
				var rest = blocks.b;
				var prepend = function (chunk) {
					return A3(
						author$project$Guide$Document$parseChunks,
						config,
						rest,
						A2(elm$core$List$cons, chunk, accumulated));
				};
				switch (first.$) {
					case 'BlankLine':
						var $temp$config = config,
							$temp$blocks = rest,
							$temp$accumulated = accumulated;
						config = $temp$config;
						blocks = $temp$blocks;
						accumulated = $temp$accumulated;
						continue parseChunks;
					case 'ThematicBreak':
						return elm$core$Result$Err('ThematicBreak not yet supported');
					case 'Heading':
						var level = first.b;
						var inlines = first.c;
						switch (level) {
							case 1:
								return elm$core$Result$Err('Heading level 1 only supported at the beginning of the document');
							case 2:
								return A2(
									elm$core$Result$andThen,
									prepend,
									A2(
										elm$core$Result$map,
										author$project$Guide$Document$Section,
										A2(author$project$Guide$Document$parseText, inlines, _List_Nil)));
							case 3:
								return A2(
									elm$core$Result$andThen,
									prepend,
									A2(
										elm$core$Result$map,
										author$project$Guide$Document$Subsection,
										A2(author$project$Guide$Document$parseText, inlines, _List_Nil)));
							default:
								return elm$core$Result$Err(
									'Heading level ' + (elm$core$String$fromInt(level) + ' not yet supported'));
						}
					case 'CodeBlock':
						if (first.a.$ === 'Indented') {
							var _n3 = first.a;
							var code = first.b;
							return prepend(
								author$project$Guide$Document$CodeBlock(code));
						} else {
							var _n4 = first.a;
							var code = first.b;
							return prepend(
								author$project$Guide$Document$CodeBlock(code));
						}
					case 'Paragraph':
						var inlines = first.b;
						return A2(
							elm$core$Result$andThen,
							A2(elm$core$Basics$composeR, author$project$Guide$Document$Paragraph, prepend),
							A2(author$project$Guide$Document$parseText, inlines, _List_Nil));
					case 'BlockQuote':
						return elm$core$Result$Err('Block quotes not yet supported');
					case 'List':
						var type_ = first.a.type_;
						var items = first.b;
						if (type_.$ === 'Unordered') {
							return A2(
								elm$core$Result$andThen,
								A2(elm$core$Basics$composeR, author$project$Guide$Document$Bullets, prepend),
								elm_community$result_extra$Result$Extra$combine(
									A2(
										elm$core$List$map,
										function (itemBlocks) {
											return A3(author$project$Guide$Document$parseChunks, config, itemBlocks, _List_Nil);
										},
										items)));
						} else {
							return elm$core$Result$Err('Numbered lists not yet supported');
						}
					case 'PlainInlines':
						var inlines = first.a;
						if (inlines.b && (!inlines.b.b)) {
							switch (inlines.a.$) {
								case 'Image':
									var _n7 = inlines.a;
									return elm$core$Result$Err('Images not yet supported');
								case 'HtmlInline':
									if ((!inlines.a.b.b) && (!inlines.a.c.b)) {
										var _n8 = inlines.a;
										var tag = _n8.a;
										var _n9 = A2(elm$core$Dict$get, tag, config.widgets);
										if (_n9.$ === 'Just') {
											var registeredWidget = _n9.a;
											return prepend(
												author$project$Guide$Document$CustomBlock(registeredWidget));
										} else {
											return elm$core$Result$Err('No widget found with name ' + tag);
										}
									} else {
										var _n10 = inlines.a;
										var tag = _n10.a;
										var attributes = _n10.b;
										var children = _n10.c;
										return elm$core$Result$Err('Custom elements with attributes or children not yet supported');
									}
								case 'Text':
									var text = inlines.a.a;
									return A2(
										elm$core$Result$andThen,
										A2(elm$core$Basics$composeR, author$project$Guide$Document$Paragraph, prepend),
										A2(author$project$Guide$Document$parseText, inlines, _List_Nil));
								default:
									var inline = inlines.a;
									return elm$core$Result$Err(
										'Only images and custom elements supported as standalone elements right now, got ' + elm$core$Debug$toString(inline));
							}
						} else {
							return A2(
								elm$core$Result$andThen,
								A2(elm$core$Basics$composeR, author$project$Guide$Document$Paragraph, prepend),
								A2(author$project$Guide$Document$parseText, inlines, _List_Nil));
						}
					default:
						return elm$core$Result$Err('Internal error - should not be possible to have a Custom block');
				}
			} else {
				return elm$core$Result$Ok(
					elm$core$List$reverse(accumulated));
			}
		}
	});
var elm$core$Dict$fromList = function (assocs) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (_n0, dict) {
				var key = _n0.a;
				var value = _n0.b;
				return A3(elm$core$Dict$insert, key, value, dict);
			}),
		elm$core$Dict$empty,
		assocs);
};
var elm$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			elm$core$List$any,
			A2(elm$core$Basics$composeL, elm$core$Basics$not, isOkay),
			list);
	});
var elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return elm$core$Maybe$Just(x);
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var elm$core$Result$fromMaybe = F2(
	function (err, maybe) {
		if (maybe.$ === 'Just') {
			var v = maybe.a;
			return elm$core$Result$Ok(v);
		} else {
			return elm$core$Result$Err(err);
		}
	});
var elm$core$Result$mapError = F2(
	function (f, result) {
		if (result.$ === 'Ok') {
			var v = result.a;
			return elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return elm$core$Result$Err(
				f(e));
		}
	});
var elm$core$Result$withDefault = F2(
	function (def, result) {
		if (result.$ === 'Ok') {
			var a = result.a;
			return a;
		} else {
			return def;
		}
	});
var elm$regex$Regex$findAtMost = _Regex_findAtMost;
var pablohirafuji$elm_markdown$Markdown$Block$BlockQuote = function (a) {
	return {$: 'BlockQuote', a: a};
};
var pablohirafuji$elm_markdown$Markdown$Block$List = F2(
	function (a, b) {
		return {$: 'List', a: a, b: b};
	});
var pablohirafuji$elm_markdown$Markdown$Block$Paragraph = F2(
	function (a, b) {
		return {$: 'Paragraph', a: a, b: b};
	});
var elm$core$String$slice = _String_slice;
var elm$core$String$right = F2(
	function (n, string) {
		return (n < 1) ? '' : A3(
			elm$core$String$slice,
			-n,
			elm$core$String$length(string),
			string);
	});
var pablohirafuji$elm_markdown$Markdown$Block$formatParagraphLine = function (rawParagraph) {
	return (A2(elm$core$String$right, 2, rawParagraph) === '  ') ? (elm$core$String$trim(rawParagraph) + '  ') : elm$core$String$trim(rawParagraph);
};
var pablohirafuji$elm_markdown$Markdown$Block$addToParagraph = F2(
	function (paragraph, rawLine) {
		return A2(
			pablohirafuji$elm_markdown$Markdown$Block$Paragraph,
			paragraph + ('\n' + pablohirafuji$elm_markdown$Markdown$Block$formatParagraphLine(rawLine)),
			_List_Nil);
	});
var pablohirafuji$elm_markdown$Markdown$Block$blockQuoteLineRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('^ {0,3}(?:>[ ]?)(.*)$'));
var elm$regex$Regex$contains = _Regex_contains;
var pablohirafuji$elm_markdown$Markdown$Block$blankLineRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('^\\s*$'));
var pablohirafuji$elm_markdown$Markdown$Block$calcListIndentLength = function (_n0) {
	var listBlock = _n0.a;
	var indentSpace = _n0.b;
	var rawLine = _n0.c;
	var indentSpaceLength = elm$core$String$length(indentSpace);
	var isIndentedCode = indentSpaceLength >= 4;
	var updtRawLine = isIndentedCode ? _Utils_ap(indentSpace, rawLine) : rawLine;
	var indentLength = (isIndentedCode || A2(elm$regex$Regex$contains, pablohirafuji$elm_markdown$Markdown$Block$blankLineRegex, rawLine)) ? (listBlock.indentLength - indentSpaceLength) : listBlock.indentLength;
	return _Utils_Tuple2(
		_Utils_update(
			listBlock,
			{indentLength: indentLength}),
		updtRawLine);
};
var elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (maybeValue.$ === 'Just') {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return elm$core$Maybe$Nothing;
		}
	});
var pablohirafuji$elm_markdown$Markdown$Block$atxHeadingLineRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('^ {0,3}(#{1,6})' + ('(?:[ \\t]+[ \\t#]+$|[ \\t]+|$)' + '(.*?)(?:\\s+[ \\t#]*)?$')));
var pablohirafuji$elm_markdown$Markdown$Block$Heading = F3(
	function (a, b, c) {
		return {$: 'Heading', a: a, b: b, c: c};
	});
var pablohirafuji$elm_markdown$Markdown$Block$extractATXHeadingRM = function (match) {
	var _n0 = match.submatches;
	if ((_n0.b && (_n0.a.$ === 'Just')) && _n0.b.b) {
		var lvl = _n0.a.a;
		var _n1 = _n0.b;
		var maybeHeading = _n1.a;
		return elm$core$Maybe$Just(
			A3(
				pablohirafuji$elm_markdown$Markdown$Block$Heading,
				A2(elm$core$Maybe$withDefault, '', maybeHeading),
				elm$core$String$length(lvl),
				_List_Nil));
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var pablohirafuji$elm_markdown$Markdown$Block$checkATXHeadingLine = function (_n0) {
	var rawLine = _n0.a;
	var ast = _n0.b;
	return A2(
		elm$core$Result$fromMaybe,
		_Utils_Tuple2(rawLine, ast),
		A2(
			elm$core$Maybe$map,
			function (a) {
				return A2(elm$core$List$cons, a, ast);
			},
			A2(
				elm$core$Maybe$andThen,
				pablohirafuji$elm_markdown$Markdown$Block$extractATXHeadingRM,
				elm$core$List$head(
					A3(elm$regex$Regex$findAtMost, 1, pablohirafuji$elm_markdown$Markdown$Block$atxHeadingLineRegex, rawLine)))));
};
var pablohirafuji$elm_markdown$Markdown$Block$BlankLine = function (a) {
	return {$: 'BlankLine', a: a};
};
var pablohirafuji$elm_markdown$Markdown$Block$CodeBlock = F2(
	function (a, b) {
		return {$: 'CodeBlock', a: a, b: b};
	});
var pablohirafuji$elm_markdown$Markdown$Block$Fenced = F2(
	function (a, b) {
		return {$: 'Fenced', a: a, b: b};
	});
var pablohirafuji$elm_markdown$Markdown$Block$addBlankLineToListBlock = F2(
	function (match, asts) {
		if (!asts.b) {
			return _List_fromArray(
				[
					_List_fromArray(
					[
						pablohirafuji$elm_markdown$Markdown$Block$BlankLine(match.match)
					])
				]);
		} else {
			var ast = asts.a;
			var astsTail = asts.b;
			return A2(
				elm$core$List$cons,
				A2(pablohirafuji$elm_markdown$Markdown$Block$parseBlankLine, ast, match),
				astsTail);
		}
	});
var pablohirafuji$elm_markdown$Markdown$Block$parseBlankLine = F2(
	function (ast, match) {
		_n0$2:
		while (true) {
			if (ast.b) {
				switch (ast.a.$) {
					case 'CodeBlock':
						if ((ast.a.a.$ === 'Fenced') && ast.a.a.a) {
							var _n1 = ast.a;
							var _n2 = _n1.a;
							var fence = _n2.b;
							var code = _n1.b;
							var astTail = ast.b;
							return function (a) {
								return A2(elm$core$List$cons, a, astTail);
							}(
								A2(
									pablohirafuji$elm_markdown$Markdown$Block$CodeBlock,
									A2(pablohirafuji$elm_markdown$Markdown$Block$Fenced, true, fence),
									code + '\n'));
						} else {
							break _n0$2;
						}
					case 'List':
						var _n3 = ast.a;
						var model = _n3.a;
						var items = _n3.b;
						var astTail = ast.b;
						return A2(
							elm$core$List$cons,
							A2(
								pablohirafuji$elm_markdown$Markdown$Block$List,
								model,
								A2(pablohirafuji$elm_markdown$Markdown$Block$addBlankLineToListBlock, match, items)),
							astTail);
					default:
						break _n0$2;
				}
			} else {
				break _n0$2;
			}
		}
		return A2(
			elm$core$List$cons,
			pablohirafuji$elm_markdown$Markdown$Block$BlankLine(match.match),
			ast);
	});
var pablohirafuji$elm_markdown$Markdown$Block$checkBlankLine = function (_n0) {
	var rawLine = _n0.a;
	var ast = _n0.b;
	return A2(
		elm$core$Result$fromMaybe,
		_Utils_Tuple2(rawLine, ast),
		A2(
			elm$core$Maybe$map,
			pablohirafuji$elm_markdown$Markdown$Block$parseBlankLine(ast),
			elm$core$List$head(
				A3(elm$regex$Regex$findAtMost, 1, pablohirafuji$elm_markdown$Markdown$Block$blankLineRegex, rawLine))));
};
var pablohirafuji$elm_markdown$Markdown$Block$indentedCodeLineRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('^(?: {4,4}| {0,3}\\t)(.*)$'));
var pablohirafuji$elm_markdown$Markdown$Block$Indented = {$: 'Indented'};
var pablohirafuji$elm_markdown$Markdown$Block$blocksAfterBlankLines = F2(
	function (ast, blankLines) {
		blocksAfterBlankLines:
		while (true) {
			if (ast.b && (ast.a.$ === 'BlankLine')) {
				var blankStr = ast.a.a;
				var astTail = ast.b;
				var $temp$ast = astTail,
					$temp$blankLines = A2(elm$core$List$cons, blankStr, blankLines);
				ast = $temp$ast;
				blankLines = $temp$blankLines;
				continue blocksAfterBlankLines;
			} else {
				return _Utils_Tuple2(ast, blankLines);
			}
		}
	});
var pablohirafuji$elm_markdown$Markdown$Block$maybeContinueParagraph = F2(
	function (rawLine, ast) {
		_n0$3:
		while (true) {
			if (ast.b) {
				switch (ast.a.$) {
					case 'Paragraph':
						var _n1 = ast.a;
						var paragraph = _n1.a;
						var astTail = ast.b;
						return elm$core$Maybe$Just(
							A2(
								elm$core$List$cons,
								A2(pablohirafuji$elm_markdown$Markdown$Block$addToParagraph, paragraph, rawLine),
								astTail));
					case 'BlockQuote':
						var bqAST = ast.a.a;
						var astTail = ast.b;
						return A2(
							elm$core$Maybe$map,
							function (updtBqAST) {
								return A2(
									elm$core$List$cons,
									pablohirafuji$elm_markdown$Markdown$Block$BlockQuote(updtBqAST),
									astTail);
							},
							A2(pablohirafuji$elm_markdown$Markdown$Block$maybeContinueParagraph, rawLine, bqAST));
					case 'List':
						var _n2 = ast.a;
						var model = _n2.a;
						var items = _n2.b;
						var astTail = ast.b;
						if (items.b) {
							var itemAST = items.a;
							var itemASTTail = items.b;
							return A2(
								elm$core$Maybe$map,
								A2(
									elm$core$Basics$composeR,
									function (a) {
										return A2(elm$core$List$cons, a, itemASTTail);
									},
									A2(
										elm$core$Basics$composeR,
										pablohirafuji$elm_markdown$Markdown$Block$List(model),
										function (a) {
											return A2(elm$core$List$cons, a, astTail);
										})),
								A2(pablohirafuji$elm_markdown$Markdown$Block$maybeContinueParagraph, rawLine, itemAST));
						} else {
							return elm$core$Maybe$Nothing;
						}
					default:
						break _n0$3;
				}
			} else {
				break _n0$3;
			}
		}
		return elm$core$Maybe$Nothing;
	});
var elm$regex$Regex$replace = _Regex_replaceAtMost(_Regex_infinity);
var elm$regex$Regex$replaceAtMost = _Regex_replaceAtMost;
var pablohirafuji$elm_markdown$Markdown$Helpers$tabRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('\\t'));
var pablohirafuji$elm_markdown$Markdown$Helpers$indentLine = function (indentLength_) {
	return A2(
		elm$core$Basics$composeR,
		A2(
			elm$regex$Regex$replace,
			pablohirafuji$elm_markdown$Markdown$Helpers$tabRegex,
			function (_n0) {
				return '    ';
			}),
		A3(
			elm$regex$Regex$replaceAtMost,
			1,
			A2(
				elm$core$Maybe$withDefault,
				elm$regex$Regex$never,
				elm$regex$Regex$fromString(
					'^ {0,' + (elm$core$String$fromInt(indentLength_) + '}'))),
			function (_n1) {
				return '';
			}));
};
var pablohirafuji$elm_markdown$Markdown$Block$resumeIndentedCodeBlock = F2(
	function (codeLine, _n0) {
		var remainBlocks = _n0.a;
		var blankLines = _n0.b;
		if ((remainBlocks.b && (remainBlocks.a.$ === 'CodeBlock')) && (remainBlocks.a.a.$ === 'Indented')) {
			var _n2 = remainBlocks.a;
			var _n3 = _n2.a;
			var codeStr = _n2.b;
			var remainBlocksTail = remainBlocks.b;
			return elm$core$Maybe$Just(
				function (a) {
					return A2(elm$core$List$cons, a, remainBlocksTail);
				}(
					A2(
						pablohirafuji$elm_markdown$Markdown$Block$CodeBlock,
						pablohirafuji$elm_markdown$Markdown$Block$Indented,
						function (a) {
							return a + (codeLine + '\n');
						}(
							_Utils_ap(
								codeStr,
								elm$core$String$concat(
									A2(
										elm$core$List$map,
										function (bl) {
											return A2(pablohirafuji$elm_markdown$Markdown$Helpers$indentLine, 4, bl) + '\n';
										},
										blankLines)))))));
		} else {
			return elm$core$Maybe$Nothing;
		}
	});
var pablohirafuji$elm_markdown$Markdown$Block$parseIndentedCodeLine = F2(
	function (ast, codeLine) {
		_n0$2:
		while (true) {
			if (ast.b) {
				switch (ast.a.$) {
					case 'CodeBlock':
						if (ast.a.a.$ === 'Indented') {
							var _n1 = ast.a;
							var _n2 = _n1.a;
							var codeStr = _n1.b;
							var astTail = ast.b;
							return function (a) {
								return A2(elm$core$List$cons, a, astTail);
							}(
								A2(pablohirafuji$elm_markdown$Markdown$Block$CodeBlock, pablohirafuji$elm_markdown$Markdown$Block$Indented, codeStr + (codeLine + '\n')));
						} else {
							break _n0$2;
						}
					case 'BlankLine':
						var blankStr = ast.a.a;
						var astTail = ast.b;
						return A2(
							elm$core$Maybe$withDefault,
							function (a) {
								return A2(elm$core$List$cons, a, ast);
							}(
								A2(pablohirafuji$elm_markdown$Markdown$Block$CodeBlock, pablohirafuji$elm_markdown$Markdown$Block$Indented, codeLine + '\n')),
							A2(
								pablohirafuji$elm_markdown$Markdown$Block$resumeIndentedCodeBlock,
								codeLine,
								A2(
									pablohirafuji$elm_markdown$Markdown$Block$blocksAfterBlankLines,
									astTail,
									_List_fromArray(
										[blankStr]))));
					default:
						break _n0$2;
				}
			} else {
				break _n0$2;
			}
		}
		return A2(
			elm$core$Maybe$withDefault,
			function (a) {
				return A2(elm$core$List$cons, a, ast);
			}(
				A2(pablohirafuji$elm_markdown$Markdown$Block$CodeBlock, pablohirafuji$elm_markdown$Markdown$Block$Indented, codeLine + '\n')),
			A2(pablohirafuji$elm_markdown$Markdown$Block$maybeContinueParagraph, codeLine, ast));
	});
var pablohirafuji$elm_markdown$Markdown$Block$checkIndentedCode = function (_n0) {
	var rawLine = _n0.a;
	var ast = _n0.b;
	return A2(
		elm$core$Result$fromMaybe,
		_Utils_Tuple2(rawLine, ast),
		A2(
			elm$core$Maybe$map,
			pablohirafuji$elm_markdown$Markdown$Block$parseIndentedCodeLine(ast),
			A2(
				elm$core$Maybe$withDefault,
				elm$core$Maybe$Nothing,
				A2(
					elm$core$Maybe$withDefault,
					elm$core$Maybe$Nothing,
					A2(
						elm$core$Maybe$map,
						A2(
							elm$core$Basics$composeR,
							function ($) {
								return $.submatches;
							},
							elm$core$List$head),
						elm$core$List$head(
							A3(elm$regex$Regex$findAtMost, 1, pablohirafuji$elm_markdown$Markdown$Block$indentedCodeLineRegex, rawLine)))))));
};
var elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3(elm$core$String$slice, 0, n, string);
	});
var pablohirafuji$elm_markdown$Markdown$Entity$decimalRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('&#([0-9]{1,8});'));
var elm$core$String$toInt = _String_toInt;
var elm$core$Char$fromCode = _Char_fromCode;
var elm$core$String$cons = _String_cons;
var elm$core$String$fromChar = function (_char) {
	return A2(elm$core$String$cons, _char, '');
};
var elm$core$Basics$modBy = _Basics_modBy;
var pablohirafuji$elm_markdown$Markdown$Entity$isBadEndUnicode = function (_int) {
	var remain_ = A2(elm$core$Basics$modBy, 16, _int);
	var remain = A2(elm$core$Basics$modBy, 131070, _int);
	return (_int >= 131070) && ((((0 <= remain) && (remain <= 15)) || ((65536 <= remain) && (remain <= 65551))) && ((remain_ === 14) || (remain_ === 15)));
};
var pablohirafuji$elm_markdown$Markdown$Entity$isValidUnicode = function (_int) {
	return (_int === 9) || ((_int === 10) || ((_int === 13) || ((_int === 133) || (((32 <= _int) && (_int <= 126)) || (((160 <= _int) && (_int <= 55295)) || (((57344 <= _int) && (_int <= 64975)) || (((65008 <= _int) && (_int <= 65533)) || ((65536 <= _int) && (_int <= 1114109)))))))));
};
var pablohirafuji$elm_markdown$Markdown$Entity$validUnicode = function (_int) {
	return (pablohirafuji$elm_markdown$Markdown$Entity$isValidUnicode(_int) && (!pablohirafuji$elm_markdown$Markdown$Entity$isBadEndUnicode(_int))) ? elm$core$String$fromChar(
		elm$core$Char$fromCode(_int)) : elm$core$String$fromChar(
		elm$core$Char$fromCode(65533));
};
var pablohirafuji$elm_markdown$Markdown$Entity$replaceDecimal = function (match) {
	return A2(
		elm$core$Maybe$withDefault,
		match.match,
		A2(
			elm$core$Maybe$map,
			pablohirafuji$elm_markdown$Markdown$Entity$validUnicode,
			A2(
				elm$core$Maybe$andThen,
				elm$core$String$toInt,
				A2(
					elm$core$Maybe$withDefault,
					elm$core$Maybe$Nothing,
					elm$core$List$head(match.submatches)))));
};
var pablohirafuji$elm_markdown$Markdown$Entity$replaceDecimals = A2(elm$regex$Regex$replace, pablohirafuji$elm_markdown$Markdown$Entity$decimalRegex, pablohirafuji$elm_markdown$Markdown$Entity$replaceDecimal);
var pablohirafuji$elm_markdown$Markdown$Entity$entitiesRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('&([0-9a-zA-Z]+);'));
var pablohirafuji$elm_markdown$Markdown$Entity$entities = elm$core$Dict$fromList(
	_List_fromArray(
		[
			_Utils_Tuple2('quot', 34),
			_Utils_Tuple2('amp', 38),
			_Utils_Tuple2('apos', 39),
			_Utils_Tuple2('lt', 60),
			_Utils_Tuple2('gt', 62),
			_Utils_Tuple2('nbsp', 160),
			_Utils_Tuple2('iexcl', 161),
			_Utils_Tuple2('cent', 162),
			_Utils_Tuple2('pound', 163),
			_Utils_Tuple2('curren', 164),
			_Utils_Tuple2('yen', 165),
			_Utils_Tuple2('brvbar', 166),
			_Utils_Tuple2('sect', 167),
			_Utils_Tuple2('uml', 168),
			_Utils_Tuple2('copy', 169),
			_Utils_Tuple2('ordf', 170),
			_Utils_Tuple2('laquo', 171),
			_Utils_Tuple2('not', 172),
			_Utils_Tuple2('shy', 173),
			_Utils_Tuple2('reg', 174),
			_Utils_Tuple2('macr', 175),
			_Utils_Tuple2('deg', 176),
			_Utils_Tuple2('plusmn', 177),
			_Utils_Tuple2('sup2', 178),
			_Utils_Tuple2('sup3', 179),
			_Utils_Tuple2('acute', 180),
			_Utils_Tuple2('micro', 181),
			_Utils_Tuple2('para', 182),
			_Utils_Tuple2('middot', 183),
			_Utils_Tuple2('cedil', 184),
			_Utils_Tuple2('sup1', 185),
			_Utils_Tuple2('ordm', 186),
			_Utils_Tuple2('raquo', 187),
			_Utils_Tuple2('frac14', 188),
			_Utils_Tuple2('frac12', 189),
			_Utils_Tuple2('frac34', 190),
			_Utils_Tuple2('iquest', 191),
			_Utils_Tuple2('Agrave', 192),
			_Utils_Tuple2('Aacute', 193),
			_Utils_Tuple2('Acirc', 194),
			_Utils_Tuple2('Atilde', 195),
			_Utils_Tuple2('Auml', 196),
			_Utils_Tuple2('Aring', 197),
			_Utils_Tuple2('AElig', 198),
			_Utils_Tuple2('Ccedil', 199),
			_Utils_Tuple2('Egrave', 200),
			_Utils_Tuple2('Eacute', 201),
			_Utils_Tuple2('Ecirc', 202),
			_Utils_Tuple2('Euml', 203),
			_Utils_Tuple2('Igrave', 204),
			_Utils_Tuple2('Iacute', 205),
			_Utils_Tuple2('Icirc', 206),
			_Utils_Tuple2('Iuml', 207),
			_Utils_Tuple2('ETH', 208),
			_Utils_Tuple2('Ntilde', 209),
			_Utils_Tuple2('Ograve', 210),
			_Utils_Tuple2('Oacute', 211),
			_Utils_Tuple2('Ocirc', 212),
			_Utils_Tuple2('Otilde', 213),
			_Utils_Tuple2('Ouml', 214),
			_Utils_Tuple2('times', 215),
			_Utils_Tuple2('Oslash', 216),
			_Utils_Tuple2('Ugrave', 217),
			_Utils_Tuple2('Uacute', 218),
			_Utils_Tuple2('Ucirc', 219),
			_Utils_Tuple2('Uuml', 220),
			_Utils_Tuple2('Yacute', 221),
			_Utils_Tuple2('THORN', 222),
			_Utils_Tuple2('szlig', 223),
			_Utils_Tuple2('agrave', 224),
			_Utils_Tuple2('aacute', 225),
			_Utils_Tuple2('acirc', 226),
			_Utils_Tuple2('atilde', 227),
			_Utils_Tuple2('auml', 228),
			_Utils_Tuple2('aring', 229),
			_Utils_Tuple2('aelig', 230),
			_Utils_Tuple2('ccedil', 231),
			_Utils_Tuple2('egrave', 232),
			_Utils_Tuple2('eacute', 233),
			_Utils_Tuple2('ecirc', 234),
			_Utils_Tuple2('euml', 235),
			_Utils_Tuple2('igrave', 236),
			_Utils_Tuple2('iacute', 237),
			_Utils_Tuple2('icirc', 238),
			_Utils_Tuple2('iuml', 239),
			_Utils_Tuple2('eth', 240),
			_Utils_Tuple2('ntilde', 241),
			_Utils_Tuple2('ograve', 242),
			_Utils_Tuple2('oacute', 243),
			_Utils_Tuple2('ocirc', 244),
			_Utils_Tuple2('otilde', 245),
			_Utils_Tuple2('ouml', 246),
			_Utils_Tuple2('divide', 247),
			_Utils_Tuple2('oslash', 248),
			_Utils_Tuple2('ugrave', 249),
			_Utils_Tuple2('uacute', 250),
			_Utils_Tuple2('ucirc', 251),
			_Utils_Tuple2('uuml', 252),
			_Utils_Tuple2('yacute', 253),
			_Utils_Tuple2('thorn', 254),
			_Utils_Tuple2('yuml', 255),
			_Utils_Tuple2('OElig', 338),
			_Utils_Tuple2('oelig', 339),
			_Utils_Tuple2('Scaron', 352),
			_Utils_Tuple2('scaron', 353),
			_Utils_Tuple2('Yuml', 376),
			_Utils_Tuple2('fnof', 402),
			_Utils_Tuple2('circ', 710),
			_Utils_Tuple2('tilde', 732),
			_Utils_Tuple2('Alpha', 913),
			_Utils_Tuple2('Beta', 914),
			_Utils_Tuple2('Gamma', 915),
			_Utils_Tuple2('Delta', 916),
			_Utils_Tuple2('Epsilon', 917),
			_Utils_Tuple2('Zeta', 918),
			_Utils_Tuple2('Eta', 919),
			_Utils_Tuple2('Theta', 920),
			_Utils_Tuple2('Iota', 921),
			_Utils_Tuple2('Kappa', 922),
			_Utils_Tuple2('Lambda', 923),
			_Utils_Tuple2('Mu', 924),
			_Utils_Tuple2('Nu', 925),
			_Utils_Tuple2('Xi', 926),
			_Utils_Tuple2('Omicron', 927),
			_Utils_Tuple2('Pi', 928),
			_Utils_Tuple2('Rho', 929),
			_Utils_Tuple2('Sigma', 931),
			_Utils_Tuple2('Tau', 932),
			_Utils_Tuple2('Upsilon', 933),
			_Utils_Tuple2('Phi', 934),
			_Utils_Tuple2('Chi', 935),
			_Utils_Tuple2('Psi', 936),
			_Utils_Tuple2('Omega', 937),
			_Utils_Tuple2('alpha', 945),
			_Utils_Tuple2('beta', 946),
			_Utils_Tuple2('gamma', 947),
			_Utils_Tuple2('delta', 948),
			_Utils_Tuple2('epsilon', 949),
			_Utils_Tuple2('zeta', 950),
			_Utils_Tuple2('eta', 951),
			_Utils_Tuple2('theta', 952),
			_Utils_Tuple2('iota', 953),
			_Utils_Tuple2('kappa', 954),
			_Utils_Tuple2('lambda', 955),
			_Utils_Tuple2('mu', 956),
			_Utils_Tuple2('nu', 957),
			_Utils_Tuple2('xi', 958),
			_Utils_Tuple2('omicron', 959),
			_Utils_Tuple2('pi', 960),
			_Utils_Tuple2('rho', 961),
			_Utils_Tuple2('sigmaf', 962),
			_Utils_Tuple2('sigma', 963),
			_Utils_Tuple2('tau', 964),
			_Utils_Tuple2('upsilon', 965),
			_Utils_Tuple2('phi', 966),
			_Utils_Tuple2('chi', 967),
			_Utils_Tuple2('psi', 968),
			_Utils_Tuple2('omega', 969),
			_Utils_Tuple2('thetasym', 977),
			_Utils_Tuple2('upsih', 978),
			_Utils_Tuple2('piv', 982),
			_Utils_Tuple2('ensp', 8194),
			_Utils_Tuple2('emsp', 8195),
			_Utils_Tuple2('thinsp', 8201),
			_Utils_Tuple2('zwnj', 8204),
			_Utils_Tuple2('zwj', 8205),
			_Utils_Tuple2('lrm', 8206),
			_Utils_Tuple2('rlm', 8207),
			_Utils_Tuple2('ndash', 8211),
			_Utils_Tuple2('mdash', 8212),
			_Utils_Tuple2('lsquo', 8216),
			_Utils_Tuple2('rsquo', 8217),
			_Utils_Tuple2('sbquo', 8218),
			_Utils_Tuple2('ldquo', 8220),
			_Utils_Tuple2('rdquo', 8221),
			_Utils_Tuple2('bdquo', 8222),
			_Utils_Tuple2('dagger', 8224),
			_Utils_Tuple2('Dagger', 8225),
			_Utils_Tuple2('bull', 8226),
			_Utils_Tuple2('hellip', 8230),
			_Utils_Tuple2('permil', 8240),
			_Utils_Tuple2('prime', 8242),
			_Utils_Tuple2('Prime', 8243),
			_Utils_Tuple2('lsaquo', 8249),
			_Utils_Tuple2('rsaquo', 8250),
			_Utils_Tuple2('oline', 8254),
			_Utils_Tuple2('frasl', 8260),
			_Utils_Tuple2('euro', 8364),
			_Utils_Tuple2('image', 8465),
			_Utils_Tuple2('weierp', 8472),
			_Utils_Tuple2('real', 8476),
			_Utils_Tuple2('trade', 8482),
			_Utils_Tuple2('alefsym', 8501),
			_Utils_Tuple2('larr', 8592),
			_Utils_Tuple2('uarr', 8593),
			_Utils_Tuple2('rarr', 8594),
			_Utils_Tuple2('darr', 8595),
			_Utils_Tuple2('harr', 8596),
			_Utils_Tuple2('crarr', 8629),
			_Utils_Tuple2('lArr', 8656),
			_Utils_Tuple2('uArr', 8657),
			_Utils_Tuple2('rArr', 8658),
			_Utils_Tuple2('dArr', 8659),
			_Utils_Tuple2('hArr', 8660),
			_Utils_Tuple2('forall', 8704),
			_Utils_Tuple2('part', 8706),
			_Utils_Tuple2('exist', 8707),
			_Utils_Tuple2('empty', 8709),
			_Utils_Tuple2('nabla', 8711),
			_Utils_Tuple2('isin', 8712),
			_Utils_Tuple2('notin', 8713),
			_Utils_Tuple2('ni', 8715),
			_Utils_Tuple2('prod', 8719),
			_Utils_Tuple2('sum', 8721),
			_Utils_Tuple2('minus', 8722),
			_Utils_Tuple2('lowast', 8727),
			_Utils_Tuple2('radic', 8730),
			_Utils_Tuple2('prop', 8733),
			_Utils_Tuple2('infin', 8734),
			_Utils_Tuple2('ang', 8736),
			_Utils_Tuple2('and', 8743),
			_Utils_Tuple2('or', 8744),
			_Utils_Tuple2('cap', 8745),
			_Utils_Tuple2('cup', 8746),
			_Utils_Tuple2('int', 8747),
			_Utils_Tuple2('there4', 8756),
			_Utils_Tuple2('sim', 8764),
			_Utils_Tuple2('cong', 8773),
			_Utils_Tuple2('asymp', 8776),
			_Utils_Tuple2('ne', 8800),
			_Utils_Tuple2('equiv', 8801),
			_Utils_Tuple2('le', 8804),
			_Utils_Tuple2('ge', 8805),
			_Utils_Tuple2('sub', 8834),
			_Utils_Tuple2('sup', 8835),
			_Utils_Tuple2('nsub', 8836),
			_Utils_Tuple2('sube', 8838),
			_Utils_Tuple2('supe', 8839),
			_Utils_Tuple2('oplus', 8853),
			_Utils_Tuple2('otimes', 8855),
			_Utils_Tuple2('perp', 8869),
			_Utils_Tuple2('sdot', 8901),
			_Utils_Tuple2('lceil', 8968),
			_Utils_Tuple2('rceil', 8969),
			_Utils_Tuple2('lfloor', 8970),
			_Utils_Tuple2('rfloor', 8971),
			_Utils_Tuple2('lang', 9001),
			_Utils_Tuple2('rang', 9002),
			_Utils_Tuple2('loz', 9674),
			_Utils_Tuple2('spades', 9824),
			_Utils_Tuple2('clubs', 9827),
			_Utils_Tuple2('hearts', 9829),
			_Utils_Tuple2('diams', 9830)
		]));
var pablohirafuji$elm_markdown$Markdown$Entity$replaceEntity = function (match) {
	return A2(
		elm$core$Maybe$withDefault,
		match.match,
		A2(
			elm$core$Maybe$map,
			A2(elm$core$Basics$composeR, elm$core$Char$fromCode, elm$core$String$fromChar),
			A2(
				elm$core$Maybe$andThen,
				function (a) {
					return A2(elm$core$Dict$get, a, pablohirafuji$elm_markdown$Markdown$Entity$entities);
				},
				A2(
					elm$core$Maybe$withDefault,
					elm$core$Maybe$Nothing,
					elm$core$List$head(match.submatches)))));
};
var pablohirafuji$elm_markdown$Markdown$Entity$replaceEntities = A2(elm$regex$Regex$replace, pablohirafuji$elm_markdown$Markdown$Entity$entitiesRegex, pablohirafuji$elm_markdown$Markdown$Entity$replaceEntity);
var pablohirafuji$elm_markdown$Markdown$Entity$hexadecimalRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('&#[Xx]([0-9a-fA-F]{1,8});'));
var elm$core$String$foldr = _String_foldr;
var elm$core$String$toList = function (string) {
	return A3(elm$core$String$foldr, elm$core$List$cons, _List_Nil, string);
};
var pablohirafuji$elm_markdown$Markdown$Entity$hexToInt = A2(
	elm$core$Basics$composeR,
	elm$core$String$toLower,
	A2(
		elm$core$Basics$composeR,
		elm$core$String$toList,
		A2(
			elm$core$List$foldl,
			F2(
				function (hexDigit, _int) {
					return ((_int * 16) + A2(
						elm$core$Basics$modBy,
						39,
						elm$core$Char$toCode(hexDigit))) - 9;
				}),
			0)));
var pablohirafuji$elm_markdown$Markdown$Entity$replaceHexadecimal = function (match) {
	return A2(
		elm$core$Maybe$withDefault,
		match.match,
		A2(
			elm$core$Maybe$map,
			A2(elm$core$Basics$composeR, pablohirafuji$elm_markdown$Markdown$Entity$hexToInt, pablohirafuji$elm_markdown$Markdown$Entity$validUnicode),
			A2(
				elm$core$Maybe$withDefault,
				elm$core$Maybe$Nothing,
				elm$core$List$head(match.submatches))));
};
var pablohirafuji$elm_markdown$Markdown$Entity$replaceHexadecimals = A2(elm$regex$Regex$replace, pablohirafuji$elm_markdown$Markdown$Entity$hexadecimalRegex, pablohirafuji$elm_markdown$Markdown$Entity$replaceHexadecimal);
var pablohirafuji$elm_markdown$Markdown$Helpers$escapableRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('(\\\\+)([!\"#$%&\\\'()*+,./:;<=>?@[\\\\\\]^_`{|}~-])'));
var pablohirafuji$elm_markdown$Markdown$Helpers$replaceEscapable = A2(
	elm$regex$Regex$replace,
	pablohirafuji$elm_markdown$Markdown$Helpers$escapableRegex,
	function (regexMatch) {
		var _n0 = regexMatch.submatches;
		if (((_n0.b && (_n0.a.$ === 'Just')) && _n0.b.b) && (_n0.b.a.$ === 'Just')) {
			var backslashes = _n0.a.a;
			var _n1 = _n0.b;
			var escapedStr = _n1.a.a;
			return _Utils_ap(
				A2(
					elm$core$String$repeat,
					(elm$core$String$length(backslashes) / 2) | 0,
					'\\'),
				escapedStr);
		} else {
			return regexMatch.match;
		}
	});
var pablohirafuji$elm_markdown$Markdown$Helpers$formatStr = function (str) {
	return pablohirafuji$elm_markdown$Markdown$Entity$replaceHexadecimals(
		pablohirafuji$elm_markdown$Markdown$Entity$replaceDecimals(
			pablohirafuji$elm_markdown$Markdown$Entity$replaceEntities(
				pablohirafuji$elm_markdown$Markdown$Helpers$replaceEscapable(str))));
};
var pablohirafuji$elm_markdown$Markdown$Block$extractOpenCodeFenceRM = function (match) {
	var _n0 = match.submatches;
	if (((_n0.b && _n0.b.b) && (_n0.b.a.$ === 'Just')) && _n0.b.b.b) {
		var maybeIndent = _n0.a;
		var _n1 = _n0.b;
		var fence = _n1.a.a;
		var _n2 = _n1.b;
		var maybeLanguage = _n2.a;
		return elm$core$Maybe$Just(
			A2(
				pablohirafuji$elm_markdown$Markdown$Block$Fenced,
				true,
				{
					fenceChar: A2(elm$core$String$left, 1, fence),
					fenceLength: elm$core$String$length(fence),
					indentLength: A2(
						elm$core$Maybe$withDefault,
						0,
						A2(elm$core$Maybe$map, elm$core$String$length, maybeIndent)),
					language: A2(
						elm$core$Maybe$map,
						pablohirafuji$elm_markdown$Markdown$Helpers$formatStr,
						A2(
							elm$core$Maybe$andThen,
							function (lang) {
								return (lang === '') ? elm$core$Maybe$Nothing : elm$core$Maybe$Just(lang);
							},
							elm$core$List$head(
								A2(
									elm$core$Maybe$withDefault,
									_List_Nil,
									A2(elm$core$Maybe$map, elm$core$String$words, maybeLanguage)))))
				}));
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var pablohirafuji$elm_markdown$Markdown$Block$openCodeFenceLineRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('^( {0,3})(`{3,}(?!.*`)|~{3,}(?!.*~))(.*)$'));
var pablohirafuji$elm_markdown$Markdown$Block$checkOpenCodeFenceLine = function (_n0) {
	var rawLine = _n0.a;
	var ast = _n0.b;
	return A2(
		elm$core$Result$fromMaybe,
		_Utils_Tuple2(rawLine, ast),
		A2(
			elm$core$Maybe$map,
			function (a) {
				return A2(elm$core$List$cons, a, ast);
			},
			A2(
				elm$core$Maybe$map,
				function (f) {
					return A2(pablohirafuji$elm_markdown$Markdown$Block$CodeBlock, f, '');
				},
				A2(
					elm$core$Maybe$andThen,
					pablohirafuji$elm_markdown$Markdown$Block$extractOpenCodeFenceRM,
					elm$core$List$head(
						A3(elm$regex$Regex$findAtMost, 1, pablohirafuji$elm_markdown$Markdown$Block$openCodeFenceLineRegex, rawLine))))));
};
var pablohirafuji$elm_markdown$Markdown$Block$Ordered = function (a) {
	return {$: 'Ordered', a: a};
};
var pablohirafuji$elm_markdown$Markdown$Block$Unordered = {$: 'Unordered'};
var pablohirafuji$elm_markdown$Markdown$Block$extractOrderedListRM = function (match) {
	var _n0 = match.submatches;
	if (((((((_n0.b && (_n0.a.$ === 'Just')) && _n0.b.b) && (_n0.b.a.$ === 'Just')) && _n0.b.b.b) && (_n0.b.b.a.$ === 'Just')) && _n0.b.b.b.b) && _n0.b.b.b.b.b) {
		var indentString = _n0.a.a;
		var _n1 = _n0.b;
		var start = _n1.a.a;
		var _n2 = _n1.b;
		var delimiter = _n2.a.a;
		var _n3 = _n2.b;
		var maybeIndentSpace = _n3.a;
		var _n4 = _n3.b;
		var maybeRawLine = _n4.a;
		return elm$core$Maybe$Just(
			_Utils_Tuple3(
				{
					delimiter: delimiter,
					indentLength: elm$core$String$length(indentString) + 1,
					isLoose: false,
					type_: A2(
						elm$core$Maybe$withDefault,
						pablohirafuji$elm_markdown$Markdown$Block$Unordered,
						A2(
							elm$core$Maybe$map,
							pablohirafuji$elm_markdown$Markdown$Block$Ordered,
							elm$core$String$toInt(start)))
				},
				A2(elm$core$Maybe$withDefault, '', maybeIndentSpace),
				A2(elm$core$Maybe$withDefault, '', maybeRawLine)));
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var pablohirafuji$elm_markdown$Markdown$Block$orderedListLineRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('^( *(\\d{1,9})([.)])( {0,4}))(?:[ \\t](.*))?$'));
var pablohirafuji$elm_markdown$Markdown$Block$checkOrderedListLine = function (rawLine) {
	return A2(
		elm$core$Result$fromMaybe,
		rawLine,
		A2(
			elm$core$Maybe$andThen,
			pablohirafuji$elm_markdown$Markdown$Block$extractOrderedListRM,
			elm$core$List$head(
				A3(elm$regex$Regex$findAtMost, 1, pablohirafuji$elm_markdown$Markdown$Block$orderedListLineRegex, rawLine))));
};
var elm$core$String$startsWith = _String_startsWith;
var pablohirafuji$elm_markdown$Markdown$Block$extractSetextHeadingRM = function (match) {
	var _n0 = match.submatches;
	if (_n0.b && (_n0.a.$ === 'Just')) {
		var delimiter = _n0.a.a;
		return A2(elm$core$String$startsWith, '=', delimiter) ? elm$core$Maybe$Just(
			_Utils_Tuple2(1, delimiter)) : elm$core$Maybe$Just(
			_Utils_Tuple2(2, delimiter));
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var pablohirafuji$elm_markdown$Markdown$Block$parseSetextHeadingLine = F3(
	function (rawLine, ast, _n0) {
		var lvl = _n0.a;
		var delimiter = _n0.b;
		if (ast.b && (ast.a.$ === 'Paragraph')) {
			var _n2 = ast.a;
			var rawText = _n2.a;
			var astTail = ast.b;
			return elm$core$Maybe$Just(
				A2(
					elm$core$List$cons,
					A3(pablohirafuji$elm_markdown$Markdown$Block$Heading, rawText, lvl, _List_Nil),
					astTail));
		} else {
			return elm$core$Maybe$Nothing;
		}
	});
var pablohirafuji$elm_markdown$Markdown$Block$setextHeadingLineRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('^ {0,3}(=+|-+)[ \\t]*$'));
var pablohirafuji$elm_markdown$Markdown$Block$checkSetextHeadingLine = function (_n0) {
	var rawLine = _n0.a;
	var ast = _n0.b;
	return A2(
		elm$core$Result$fromMaybe,
		_Utils_Tuple2(rawLine, ast),
		A2(
			elm$core$Maybe$andThen,
			A2(pablohirafuji$elm_markdown$Markdown$Block$parseSetextHeadingLine, rawLine, ast),
			A2(
				elm$core$Maybe$andThen,
				pablohirafuji$elm_markdown$Markdown$Block$extractSetextHeadingRM,
				elm$core$List$head(
					A3(elm$regex$Regex$findAtMost, 1, pablohirafuji$elm_markdown$Markdown$Block$setextHeadingLineRegex, rawLine)))));
};
var pablohirafuji$elm_markdown$Markdown$Block$ThematicBreak = {$: 'ThematicBreak'};
var pablohirafuji$elm_markdown$Markdown$Block$thematicBreakLineRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('^ {0,3}(?:' + ('(?:\\*[ \\t]*){3,}' + ('|(?:_[ \\t]*){3,}' + '|(?:-[ \\t]*){3,})[ \\t]*$'))));
var pablohirafuji$elm_markdown$Markdown$Block$checkThematicBreakLine = function (_n0) {
	var rawLine = _n0.a;
	var ast = _n0.b;
	return A2(
		elm$core$Result$fromMaybe,
		_Utils_Tuple2(rawLine, ast),
		A2(
			elm$core$Maybe$map,
			function (_n1) {
				return A2(elm$core$List$cons, pablohirafuji$elm_markdown$Markdown$Block$ThematicBreak, ast);
			},
			elm$core$List$head(
				A3(elm$regex$Regex$findAtMost, 1, pablohirafuji$elm_markdown$Markdown$Block$thematicBreakLineRegex, rawLine))));
};
var pablohirafuji$elm_markdown$Markdown$Block$extractUnorderedListRM = function (match) {
	var _n0 = match.submatches;
	if ((((((_n0.b && (_n0.a.$ === 'Just')) && _n0.b.b) && (_n0.b.a.$ === 'Just')) && _n0.b.b.b) && _n0.b.b.b.b) && (!_n0.b.b.b.b.b)) {
		var indentString = _n0.a.a;
		var _n1 = _n0.b;
		var delimiter = _n1.a.a;
		var _n2 = _n1.b;
		var maybeIndentSpace = _n2.a;
		var _n3 = _n2.b;
		var maybeRawLine = _n3.a;
		return elm$core$Maybe$Just(
			_Utils_Tuple3(
				{
					delimiter: delimiter,
					indentLength: elm$core$String$length(indentString) + 1,
					isLoose: false,
					type_: pablohirafuji$elm_markdown$Markdown$Block$Unordered
				},
				A2(elm$core$Maybe$withDefault, '', maybeIndentSpace),
				A2(elm$core$Maybe$withDefault, '', maybeRawLine)));
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var pablohirafuji$elm_markdown$Markdown$Block$unorderedListLineRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('^( *([\\*\\-\\+])( {0,4}))(?:[ \\t](.*))?$'));
var pablohirafuji$elm_markdown$Markdown$Block$checkUnorderedListLine = function (rawLine) {
	return A2(
		elm$core$Result$fromMaybe,
		rawLine,
		A2(
			elm$core$Maybe$andThen,
			pablohirafuji$elm_markdown$Markdown$Block$extractUnorderedListRM,
			elm$core$List$head(
				A3(elm$regex$Regex$findAtMost, 1, pablohirafuji$elm_markdown$Markdown$Block$unorderedListLineRegex, rawLine))));
};
var pablohirafuji$elm_markdown$Markdown$Block$closeCodeFenceLineRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('^ {0,3}(`{3,}|~{3,})\\s*$'));
var pablohirafuji$elm_markdown$Markdown$Block$isCloseFenceLineHelp = F2(
	function (fence, match) {
		var _n0 = match.submatches;
		if (_n0.b && (_n0.a.$ === 'Just')) {
			var fenceStr = _n0.a.a;
			return (_Utils_cmp(
				elm$core$String$length(fenceStr),
				fence.fenceLength) > -1) && _Utils_eq(
				A2(elm$core$String$left, 1, fenceStr),
				fence.fenceChar);
		} else {
			return false;
		}
	});
var pablohirafuji$elm_markdown$Markdown$Block$isCloseFenceLine = function (fence) {
	return A2(
		elm$core$Basics$composeR,
		A2(elm$regex$Regex$findAtMost, 1, pablohirafuji$elm_markdown$Markdown$Block$closeCodeFenceLineRegex),
		A2(
			elm$core$Basics$composeR,
			elm$core$List$head,
			A2(
				elm$core$Basics$composeR,
				elm$core$Maybe$map(
					pablohirafuji$elm_markdown$Markdown$Block$isCloseFenceLineHelp(fence)),
				elm$core$Maybe$withDefault(false))));
};
var pablohirafuji$elm_markdown$Markdown$Block$continueOrCloseCodeFence = F3(
	function (fence, previousCode, rawLine) {
		return A2(pablohirafuji$elm_markdown$Markdown$Block$isCloseFenceLine, fence, rawLine) ? A2(
			pablohirafuji$elm_markdown$Markdown$Block$CodeBlock,
			A2(pablohirafuji$elm_markdown$Markdown$Block$Fenced, false, fence),
			previousCode) : A2(
			pablohirafuji$elm_markdown$Markdown$Block$CodeBlock,
			A2(pablohirafuji$elm_markdown$Markdown$Block$Fenced, true, fence),
			previousCode + (A2(pablohirafuji$elm_markdown$Markdown$Helpers$indentLine, fence.indentLength, rawLine) + '\n'));
	});
var pablohirafuji$elm_markdown$Markdown$Block$isBlankLineLast = function (items) {
	isBlankLineLast:
	while (true) {
		if (!items.b) {
			return false;
		} else {
			var item = items.a;
			var itemsTail = items.b;
			_n1$3:
			while (true) {
				if (item.b) {
					switch (item.a.$) {
						case 'BlankLine':
							if (!item.b.b) {
								return false;
							} else {
								return true;
							}
						case 'List':
							var _n2 = item.a;
							var items_ = _n2.b;
							var $temp$items = items_;
							items = $temp$items;
							continue isBlankLineLast;
						default:
							break _n1$3;
					}
				} else {
					break _n1$3;
				}
			}
			return false;
		}
	}
};
var pablohirafuji$elm_markdown$Markdown$Block$parseTextLine = F2(
	function (rawLine, ast) {
		return A2(
			elm$core$Maybe$withDefault,
			A2(
				elm$core$List$cons,
				A2(
					pablohirafuji$elm_markdown$Markdown$Block$Paragraph,
					pablohirafuji$elm_markdown$Markdown$Block$formatParagraphLine(rawLine),
					_List_Nil),
				ast),
			A2(pablohirafuji$elm_markdown$Markdown$Block$maybeContinueParagraph, rawLine, ast));
	});
var pablohirafuji$elm_markdown$Markdown$Helpers$ifError = F2(
	function (_function, result) {
		if (result.$ === 'Ok') {
			return result;
		} else {
			var err = result.a;
			return _function(err);
		}
	});
var pablohirafuji$elm_markdown$Markdown$Helpers$initSpacesRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('^ +'));
var pablohirafuji$elm_markdown$Markdown$Helpers$indentLength = A2(
	elm$core$Basics$composeR,
	A2(
		elm$regex$Regex$replace,
		pablohirafuji$elm_markdown$Markdown$Helpers$tabRegex,
		function (_n0) {
			return '    ';
		}),
	A2(
		elm$core$Basics$composeR,
		A2(elm$regex$Regex$findAtMost, 1, pablohirafuji$elm_markdown$Markdown$Helpers$initSpacesRegex),
		A2(
			elm$core$Basics$composeR,
			elm$core$List$head,
			A2(
				elm$core$Basics$composeR,
				elm$core$Maybe$map(
					A2(
						elm$core$Basics$composeR,
						function ($) {
							return $.match;
						},
						elm$core$String$length)),
				elm$core$Maybe$withDefault(0)))));
var pablohirafuji$elm_markdown$Markdown$Block$checkBlockQuote = function (_n16) {
	var rawLine = _n16.a;
	var ast = _n16.b;
	return A2(
		elm$core$Result$fromMaybe,
		_Utils_Tuple2(rawLine, ast),
		A2(
			elm$core$Maybe$map,
			pablohirafuji$elm_markdown$Markdown$Block$parseBlockQuoteLine(ast),
			A2(
				elm$core$Maybe$map,
				A2(
					elm$core$Basics$composeR,
					function ($) {
						return $.submatches;
					},
					A2(
						elm$core$Basics$composeR,
						elm$core$List$head,
						A2(
							elm$core$Basics$composeR,
							elm$core$Maybe$withDefault(elm$core$Maybe$Nothing),
							elm$core$Maybe$withDefault('')))),
				elm$core$List$head(
					A3(elm$regex$Regex$findAtMost, 1, pablohirafuji$elm_markdown$Markdown$Block$blockQuoteLineRegex, rawLine)))));
};
var pablohirafuji$elm_markdown$Markdown$Block$checkListLine = function (_n15) {
	var rawLine = _n15.a;
	var ast = _n15.b;
	return A2(
		elm$core$Result$mapError,
		function (e) {
			return _Utils_Tuple2(e, ast);
		},
		A2(
			elm$core$Result$map,
			A2(pablohirafuji$elm_markdown$Markdown$Block$parseListLine, rawLine, ast),
			A2(
				elm$core$Result$map,
				pablohirafuji$elm_markdown$Markdown$Block$calcListIndentLength,
				A2(
					pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
					pablohirafuji$elm_markdown$Markdown$Block$checkUnorderedListLine,
					pablohirafuji$elm_markdown$Markdown$Block$checkOrderedListLine(rawLine)))));
};
var pablohirafuji$elm_markdown$Markdown$Block$incorporateLine = F2(
	function (rawLine, ast) {
		_n11$2:
		while (true) {
			if (ast.b) {
				switch (ast.a.$) {
					case 'CodeBlock':
						if ((ast.a.a.$ === 'Fenced') && ast.a.a.a) {
							var _n12 = ast.a;
							var _n13 = _n12.a;
							var fence = _n13.b;
							var code = _n12.b;
							var astTail = ast.b;
							return function (a) {
								return A2(elm$core$List$cons, a, astTail);
							}(
								A3(pablohirafuji$elm_markdown$Markdown$Block$continueOrCloseCodeFence, fence, code, rawLine));
						} else {
							break _n11$2;
						}
					case 'List':
						var _n14 = ast.a;
						var model = _n14.a;
						var items = _n14.b;
						var astTail = ast.b;
						return (_Utils_cmp(
							pablohirafuji$elm_markdown$Markdown$Helpers$indentLength(rawLine),
							model.indentLength) > -1) ? A5(pablohirafuji$elm_markdown$Markdown$Block$parseIndentedListLine, rawLine, model, items, ast, astTail) : A2(
							elm$core$Result$withDefault,
							A2(pablohirafuji$elm_markdown$Markdown$Block$parseTextLine, rawLine, ast),
							A2(
								pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
								pablohirafuji$elm_markdown$Markdown$Block$checkBlockQuote,
								A2(
									pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
									pablohirafuji$elm_markdown$Markdown$Block$checkATXHeadingLine,
									A2(
										pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
										pablohirafuji$elm_markdown$Markdown$Block$checkSetextHeadingLine,
										A2(
											pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
											pablohirafuji$elm_markdown$Markdown$Block$checkOpenCodeFenceLine,
											A2(
												pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
												pablohirafuji$elm_markdown$Markdown$Block$checkIndentedCode,
												A2(
													pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
													pablohirafuji$elm_markdown$Markdown$Block$checkBlankLine,
													A2(
														pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
														pablohirafuji$elm_markdown$Markdown$Block$checkListLine,
														pablohirafuji$elm_markdown$Markdown$Block$checkThematicBreakLine(
															_Utils_Tuple2(rawLine, ast))))))))));
					default:
						break _n11$2;
				}
			} else {
				break _n11$2;
			}
		}
		return A2(pablohirafuji$elm_markdown$Markdown$Block$parseRawLine, rawLine, ast);
	});
var pablohirafuji$elm_markdown$Markdown$Block$parseBlockQuoteLine = F2(
	function (ast, rawLine) {
		if (ast.b && (ast.a.$ === 'BlockQuote')) {
			var bqAST = ast.a.a;
			var astTail = ast.b;
			return function (a) {
				return A2(elm$core$List$cons, a, astTail);
			}(
				pablohirafuji$elm_markdown$Markdown$Block$BlockQuote(
					A2(pablohirafuji$elm_markdown$Markdown$Block$incorporateLine, rawLine, bqAST)));
		} else {
			return function (a) {
				return A2(elm$core$List$cons, a, ast);
			}(
				pablohirafuji$elm_markdown$Markdown$Block$BlockQuote(
					A2(pablohirafuji$elm_markdown$Markdown$Block$incorporateLine, rawLine, _List_Nil)));
		}
	});
var pablohirafuji$elm_markdown$Markdown$Block$parseIndentedListLine = F5(
	function (rawLine, model, items, ast, astTail) {
		if (!items.b) {
			return function (a) {
				return A2(elm$core$List$cons, a, astTail);
			}(
				A2(
					pablohirafuji$elm_markdown$Markdown$Block$List,
					model,
					function (a) {
						return A2(elm$core$List$cons, a, _List_Nil);
					}(
						function (a) {
							return A2(pablohirafuji$elm_markdown$Markdown$Block$incorporateLine, a, _List_Nil);
						}(
							A2(pablohirafuji$elm_markdown$Markdown$Helpers$indentLine, model.indentLength, rawLine)))));
		} else {
			var item = items.a;
			var itemsTail = items.b;
			var indentedRawLine = A2(pablohirafuji$elm_markdown$Markdown$Helpers$indentLine, model.indentLength, rawLine);
			var updateList = function (model_) {
				return function (a) {
					return A2(elm$core$List$cons, a, astTail);
				}(
					A2(
						pablohirafuji$elm_markdown$Markdown$Block$List,
						model_,
						function (a) {
							return A2(elm$core$List$cons, a, itemsTail);
						}(
							A2(pablohirafuji$elm_markdown$Markdown$Block$incorporateLine, indentedRawLine, item))));
			};
			_n7$3:
			while (true) {
				if (item.b) {
					switch (item.a.$) {
						case 'BlankLine':
							if (!item.b.b) {
								return updateList(model);
							} else {
								var itemTail = item.b;
								return A2(
									elm$core$List$all,
									function (block) {
										if (block.$ === 'BlankLine') {
											return true;
										} else {
											return false;
										}
									},
									itemTail) ? A2(pablohirafuji$elm_markdown$Markdown$Block$parseRawLine, rawLine, ast) : updateList(
									_Utils_update(
										model,
										{isLoose: true}));
							}
						case 'List':
							var _n9 = item.a;
							var model_ = _n9.a;
							var items_ = _n9.b;
							var itemTail = item.b;
							return (_Utils_cmp(
								pablohirafuji$elm_markdown$Markdown$Helpers$indentLength(indentedRawLine),
								model_.indentLength) > -1) ? updateList(model) : (pablohirafuji$elm_markdown$Markdown$Block$isBlankLineLast(items_) ? updateList(
								_Utils_update(
									model,
									{isLoose: true})) : updateList(model));
						default:
							break _n7$3;
					}
				} else {
					break _n7$3;
				}
			}
			return updateList(model);
		}
	});
var pablohirafuji$elm_markdown$Markdown$Block$parseListLine = F3(
	function (rawLine, ast, _n0) {
		var listBlock = _n0.a;
		var listRawLine = _n0.b;
		var parsedRawLine = A2(pablohirafuji$elm_markdown$Markdown$Block$incorporateLine, listRawLine, _List_Nil);
		var newList = A2(
			elm$core$List$cons,
			A2(
				pablohirafuji$elm_markdown$Markdown$Block$List,
				listBlock,
				_List_fromArray(
					[parsedRawLine])),
			ast);
		_n1$2:
		while (true) {
			if (ast.b) {
				switch (ast.a.$) {
					case 'List':
						var _n2 = ast.a;
						var model = _n2.a;
						var items = _n2.b;
						var astTail = ast.b;
						return _Utils_eq(listBlock.delimiter, model.delimiter) ? function (a) {
							return A2(elm$core$List$cons, a, astTail);
						}(
							A2(
								pablohirafuji$elm_markdown$Markdown$Block$List,
								_Utils_update(
									model,
									{
										indentLength: listBlock.indentLength,
										isLoose: model.isLoose || pablohirafuji$elm_markdown$Markdown$Block$isBlankLineLast(items)
									}),
								A2(elm$core$List$cons, parsedRawLine, items))) : newList;
					case 'Paragraph':
						var _n3 = ast.a;
						var rawText = _n3.a;
						var inlines = _n3.b;
						var astTail = ast.b;
						if ((parsedRawLine.b && (parsedRawLine.a.$ === 'BlankLine')) && (!parsedRawLine.b.b)) {
							return A2(
								elm$core$List$cons,
								A2(pablohirafuji$elm_markdown$Markdown$Block$addToParagraph, rawText, rawLine),
								astTail);
						} else {
							var _n5 = listBlock.type_;
							if (_n5.$ === 'Ordered') {
								if (_n5.a === 1) {
									return newList;
								} else {
									var _int = _n5.a;
									return A2(
										elm$core$List$cons,
										A2(pablohirafuji$elm_markdown$Markdown$Block$addToParagraph, rawText, rawLine),
										astTail);
								}
							} else {
								return newList;
							}
						}
					default:
						break _n1$2;
				}
			} else {
				break _n1$2;
			}
		}
		return newList;
	});
var pablohirafuji$elm_markdown$Markdown$Block$parseRawLine = F2(
	function (rawLine, ast) {
		return A2(
			elm$core$Result$withDefault,
			A2(pablohirafuji$elm_markdown$Markdown$Block$parseTextLine, rawLine, ast),
			A2(
				pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
				pablohirafuji$elm_markdown$Markdown$Block$checkListLine,
				A2(
					pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
					pablohirafuji$elm_markdown$Markdown$Block$checkThematicBreakLine,
					A2(
						pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
						pablohirafuji$elm_markdown$Markdown$Block$checkBlockQuote,
						A2(
							pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
							pablohirafuji$elm_markdown$Markdown$Block$checkATXHeadingLine,
							A2(
								pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
								pablohirafuji$elm_markdown$Markdown$Block$checkSetextHeadingLine,
								A2(
									pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
									pablohirafuji$elm_markdown$Markdown$Block$checkOpenCodeFenceLine,
									A2(
										pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
										pablohirafuji$elm_markdown$Markdown$Block$checkIndentedCode,
										pablohirafuji$elm_markdown$Markdown$Block$checkBlankLine(
											_Utils_Tuple2(rawLine, ast))))))))));
	});
var pablohirafuji$elm_markdown$Markdown$Block$incorporateLines = F2(
	function (rawLines, ast) {
		if (!rawLines.b) {
			return ast;
		} else {
			var rawLine = rawLines.a;
			var rawLinesTail = rawLines.b;
			return A2(
				pablohirafuji$elm_markdown$Markdown$Block$incorporateLines,
				rawLinesTail,
				A2(pablohirafuji$elm_markdown$Markdown$Block$incorporateLine, rawLine, ast));
		}
	});
var pablohirafuji$elm_markdown$Markdown$Block$Custom = F2(
	function (a, b) {
		return {$: 'Custom', a: a, b: b};
	});
var pablohirafuji$elm_markdown$Markdown$Block$PlainInlines = function (a) {
	return {$: 'PlainInlines', a: a};
};
var pablohirafuji$elm_markdown$Markdown$Config$Sanitize = function (a) {
	return {$: 'Sanitize', a: a};
};
var pablohirafuji$elm_markdown$Markdown$Config$defaultAllowedHtmlAttributes = _List_fromArray(
	['name', 'class']);
var pablohirafuji$elm_markdown$Markdown$Config$defaultAllowedHtmlElements = _List_fromArray(
	['address', 'article', 'aside', 'b', 'blockquote', 'br', 'caption', 'center', 'cite', 'code', 'col', 'colgroup', 'dd', 'details', 'div', 'dl', 'dt', 'figcaption', 'figure', 'footer', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'hr', 'i', 'legend', 'li', 'menu', 'menuitem', 'nav', 'ol', 'optgroup', 'option', 'p', 'pre', 'section', 'strike', 'summary', 'small', 'table', 'tbody', 'td', 'tfoot', 'th', 'thead', 'tr', 'ul']);
var pablohirafuji$elm_markdown$Markdown$Config$defaultSanitizeOptions = {allowedHtmlAttributes: pablohirafuji$elm_markdown$Markdown$Config$defaultAllowedHtmlAttributes, allowedHtmlElements: pablohirafuji$elm_markdown$Markdown$Config$defaultAllowedHtmlElements};
var pablohirafuji$elm_markdown$Markdown$Config$defaultOptions = {
	rawHtml: pablohirafuji$elm_markdown$Markdown$Config$Sanitize(pablohirafuji$elm_markdown$Markdown$Config$defaultSanitizeOptions),
	softAsHardLineBreak: false
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$initParser = F3(
	function (options, refs, rawText) {
		return {matches: _List_Nil, options: options, rawText: rawText, refs: refs, tokens: _List_Nil};
	});
var pablohirafuji$elm_markdown$Markdown$Inline$CodeInline = function (a) {
	return {$: 'CodeInline', a: a};
};
var pablohirafuji$elm_markdown$Markdown$Inline$Emphasis = F2(
	function (a, b) {
		return {$: 'Emphasis', a: a, b: b};
	});
var pablohirafuji$elm_markdown$Markdown$Inline$HardLineBreak = {$: 'HardLineBreak'};
var pablohirafuji$elm_markdown$Markdown$Inline$HtmlInline = F3(
	function (a, b, c) {
		return {$: 'HtmlInline', a: a, b: b, c: c};
	});
var pablohirafuji$elm_markdown$Markdown$Inline$Image = F3(
	function (a, b, c) {
		return {$: 'Image', a: a, b: b, c: c};
	});
var pablohirafuji$elm_markdown$Markdown$Inline$Link = F3(
	function (a, b, c) {
		return {$: 'Link', a: a, b: b, c: c};
	});
var pablohirafuji$elm_markdown$Markdown$Inline$Text = function (a) {
	return {$: 'Text', a: a};
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$matchToInline = function (_n0) {
	var match = _n0.a;
	var _n1 = match.type_;
	switch (_n1.$) {
		case 'NormalType':
			return pablohirafuji$elm_markdown$Markdown$Inline$Text(match.text);
		case 'HardLineBreakType':
			return pablohirafuji$elm_markdown$Markdown$Inline$HardLineBreak;
		case 'CodeType':
			return pablohirafuji$elm_markdown$Markdown$Inline$CodeInline(match.text);
		case 'AutolinkType':
			var _n2 = _n1.a;
			var text = _n2.a;
			var url = _n2.b;
			return A3(
				pablohirafuji$elm_markdown$Markdown$Inline$Link,
				url,
				elm$core$Maybe$Nothing,
				_List_fromArray(
					[
						pablohirafuji$elm_markdown$Markdown$Inline$Text(text)
					]));
		case 'LinkType':
			var _n3 = _n1.a;
			var url = _n3.a;
			var maybeTitle = _n3.b;
			return A3(
				pablohirafuji$elm_markdown$Markdown$Inline$Link,
				url,
				maybeTitle,
				pablohirafuji$elm_markdown$Markdown$InlineParser$matchesToInlines(match.matches));
		case 'ImageType':
			var _n4 = _n1.a;
			var url = _n4.a;
			var maybeTitle = _n4.b;
			return A3(
				pablohirafuji$elm_markdown$Markdown$Inline$Image,
				url,
				maybeTitle,
				pablohirafuji$elm_markdown$Markdown$InlineParser$matchesToInlines(match.matches));
		case 'HtmlType':
			var model = _n1.a;
			return A3(
				pablohirafuji$elm_markdown$Markdown$Inline$HtmlInline,
				model.tag,
				model.attributes,
				pablohirafuji$elm_markdown$Markdown$InlineParser$matchesToInlines(match.matches));
		default:
			var length = _n1.a;
			return A2(
				pablohirafuji$elm_markdown$Markdown$Inline$Emphasis,
				length,
				pablohirafuji$elm_markdown$Markdown$InlineParser$matchesToInlines(match.matches));
	}
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$matchesToInlines = function (matches) {
	return A2(elm$core$List$map, pablohirafuji$elm_markdown$Markdown$InlineParser$matchToInline, matches);
};
var elm$core$List$sortBy = _List_sortBy;
var pablohirafuji$elm_markdown$Markdown$InlineParser$Match = function (a) {
	return {$: 'Match', a: a};
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$prepareChildMatch = F2(
	function (parentMatch, childMatch) {
		return pablohirafuji$elm_markdown$Markdown$InlineParser$Match(
			_Utils_update(
				childMatch,
				{end: childMatch.end - parentMatch.textStart, start: childMatch.start - parentMatch.textStart, textEnd: childMatch.textEnd - parentMatch.textStart, textStart: childMatch.textStart - parentMatch.textStart}));
	});
var pablohirafuji$elm_markdown$Markdown$InlineParser$addChild = F2(
	function (parentMatch, childMatch) {
		return pablohirafuji$elm_markdown$Markdown$InlineParser$Match(
			_Utils_update(
				parentMatch,
				{
					matches: A2(
						elm$core$List$cons,
						A2(pablohirafuji$elm_markdown$Markdown$InlineParser$prepareChildMatch, parentMatch, childMatch),
						parentMatch.matches)
				}));
	});
var pablohirafuji$elm_markdown$Markdown$InlineParser$organizeMatch = F2(
	function (_n0, matches) {
		var match = _n0.a;
		if (!matches.b) {
			return _List_fromArray(
				[
					pablohirafuji$elm_markdown$Markdown$InlineParser$Match(match)
				]);
		} else {
			var prevMatch = matches.a.a;
			var matchesTail = matches.b;
			return (_Utils_cmp(prevMatch.end, match.start) < 1) ? A2(
				elm$core$List$cons,
				pablohirafuji$elm_markdown$Markdown$InlineParser$Match(match),
				matches) : (((_Utils_cmp(prevMatch.start, match.start) < 0) && (_Utils_cmp(prevMatch.end, match.end) > 0)) ? A2(
				elm$core$List$cons,
				A2(pablohirafuji$elm_markdown$Markdown$InlineParser$addChild, prevMatch, match),
				matchesTail) : matches);
		}
	});
function pablohirafuji$elm_markdown$Markdown$InlineParser$cyclic$organizeMatches() {
	return A2(
		elm$core$Basics$composeR,
		elm$core$List$sortBy(
			function (_n0) {
				var match = _n0.a;
				return match.start;
			}),
		A2(
			elm$core$Basics$composeR,
			A2(elm$core$List$foldl, pablohirafuji$elm_markdown$Markdown$InlineParser$organizeMatch, _List_Nil),
			elm$core$List$map(
				function (_n1) {
					var match = _n1.a;
					return pablohirafuji$elm_markdown$Markdown$InlineParser$Match(
						_Utils_update(
							match,
							{
								matches: pablohirafuji$elm_markdown$Markdown$InlineParser$cyclic$organizeMatches()(match.matches)
							}));
				})));
}
try {
	var pablohirafuji$elm_markdown$Markdown$InlineParser$organizeMatches = pablohirafuji$elm_markdown$Markdown$InlineParser$cyclic$organizeMatches();
	pablohirafuji$elm_markdown$Markdown$InlineParser$cyclic$organizeMatches = function () {
		return pablohirafuji$elm_markdown$Markdown$InlineParser$organizeMatches;
	};
} catch ($) {
throw 'Some top-level definitions from `Markdown.InlineParser` are causing infinite recursion:\n\n  ┌─────┐\n  │    organizeMatches\n  └─────┘\n\nThese errors are very tricky, so read https://elm-lang.org/0.19.0/halting-problem to learn how to fix it!';}
var pablohirafuji$elm_markdown$Markdown$InlineParser$organizeParserMatches = function (model) {
	return _Utils_update(
		model,
		{
			matches: pablohirafuji$elm_markdown$Markdown$InlineParser$organizeMatches(model.matches)
		});
};
var elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			elm$core$String$slice,
			n,
			elm$core$String$length(string),
			string);
	});
var elm$core$String$isEmpty = function (string) {
	return string === '';
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$NormalType = {$: 'NormalType'};
var pablohirafuji$elm_markdown$Markdown$InlineParser$normalMatch = function (text) {
	return pablohirafuji$elm_markdown$Markdown$InlineParser$Match(
		{
			end: 0,
			matches: _List_Nil,
			start: 0,
			text: pablohirafuji$elm_markdown$Markdown$Helpers$formatStr(text),
			textEnd: 0,
			textStart: 0,
			type_: pablohirafuji$elm_markdown$Markdown$InlineParser$NormalType
		});
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$parseTextMatch = F3(
	function (rawText, _n2, parsedMatches) {
		var matchModel = _n2.a;
		var updtMatch = pablohirafuji$elm_markdown$Markdown$InlineParser$Match(
			_Utils_update(
				matchModel,
				{
					matches: A3(pablohirafuji$elm_markdown$Markdown$InlineParser$parseTextMatches, matchModel.text, _List_Nil, matchModel.matches)
				}));
		if (!parsedMatches.b) {
			var finalStr = A2(elm$core$String$dropLeft, matchModel.end, rawText);
			return elm$core$String$isEmpty(finalStr) ? _List_fromArray(
				[updtMatch]) : _List_fromArray(
				[
					updtMatch,
					pablohirafuji$elm_markdown$Markdown$InlineParser$normalMatch(finalStr)
				]);
		} else {
			var matchHead = parsedMatches.a.a;
			var matchesTail = parsedMatches.b;
			return _Utils_eq(matchHead.type_, pablohirafuji$elm_markdown$Markdown$InlineParser$NormalType) ? A2(elm$core$List$cons, updtMatch, parsedMatches) : (_Utils_eq(matchModel.end, matchHead.start) ? A2(elm$core$List$cons, updtMatch, parsedMatches) : ((_Utils_cmp(matchModel.end, matchHead.start) < 0) ? A2(
				elm$core$List$cons,
				updtMatch,
				A2(
					elm$core$List$cons,
					pablohirafuji$elm_markdown$Markdown$InlineParser$normalMatch(
						A3(elm$core$String$slice, matchModel.end, matchHead.start, rawText)),
					parsedMatches)) : parsedMatches));
		}
	});
var pablohirafuji$elm_markdown$Markdown$InlineParser$parseTextMatches = F3(
	function (rawText, parsedMatches, matches) {
		parseTextMatches:
		while (true) {
			if (!matches.b) {
				if (!parsedMatches.b) {
					return elm$core$String$isEmpty(rawText) ? _List_Nil : _List_fromArray(
						[
							pablohirafuji$elm_markdown$Markdown$InlineParser$normalMatch(rawText)
						]);
				} else {
					var matchModel = parsedMatches.a.a;
					return (matchModel.start > 0) ? A2(
						elm$core$List$cons,
						pablohirafuji$elm_markdown$Markdown$InlineParser$normalMatch(
							A2(elm$core$String$left, matchModel.start, rawText)),
						parsedMatches) : parsedMatches;
				}
			} else {
				var match = matches.a;
				var matchesTail = matches.b;
				var $temp$rawText = rawText,
					$temp$parsedMatches = A3(pablohirafuji$elm_markdown$Markdown$InlineParser$parseTextMatch, rawText, match, parsedMatches),
					$temp$matches = matchesTail;
				rawText = $temp$rawText;
				parsedMatches = $temp$parsedMatches;
				matches = $temp$matches;
				continue parseTextMatches;
			}
		}
	});
var pablohirafuji$elm_markdown$Markdown$InlineParser$parseText = function (model) {
	return _Utils_update(
		model,
		{
			matches: A3(pablohirafuji$elm_markdown$Markdown$InlineParser$parseTextMatches, model.rawText, _List_Nil, model.matches)
		});
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$angleBracketLTokenRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('(\\\\*)(\\<)'));
var pablohirafuji$elm_markdown$Markdown$Helpers$isEven = function (_int) {
	return !A2(elm$core$Basics$modBy, 2, _int);
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$CharToken = function (a) {
	return {$: 'CharToken', a: a};
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$regMatchToAngleBracketLToken = function (regMatch) {
	var _n0 = regMatch.submatches;
	if ((_n0.b && _n0.b.b) && (_n0.b.a.$ === 'Just')) {
		var maybeBackslashes = _n0.a;
		var _n1 = _n0.b;
		var delimiter = _n1.a.a;
		var backslashesLength = A2(
			elm$core$Maybe$withDefault,
			0,
			A2(elm$core$Maybe$map, elm$core$String$length, maybeBackslashes));
		return pablohirafuji$elm_markdown$Markdown$Helpers$isEven(backslashesLength) ? elm$core$Maybe$Just(
			{
				index: regMatch.index + backslashesLength,
				length: 1,
				meaning: pablohirafuji$elm_markdown$Markdown$InlineParser$CharToken(
					_Utils_chr('<'))
			}) : elm$core$Maybe$Nothing;
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$findAngleBracketLTokens = function (str) {
	return A2(
		elm$core$List$filterMap,
		pablohirafuji$elm_markdown$Markdown$InlineParser$regMatchToAngleBracketLToken,
		A2(elm$regex$Regex$find, pablohirafuji$elm_markdown$Markdown$InlineParser$angleBracketLTokenRegex, str));
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$angleBracketRTokenRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('(\\\\*)(\\>)'));
var pablohirafuji$elm_markdown$Markdown$InlineParser$RightAngleBracket = function (a) {
	return {$: 'RightAngleBracket', a: a};
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$regMatchToAngleBracketRToken = function (regMatch) {
	var _n0 = regMatch.submatches;
	if ((_n0.b && _n0.b.b) && (_n0.b.a.$ === 'Just')) {
		var maybeBackslashes = _n0.a;
		var _n1 = _n0.b;
		var backslashesLength = A2(
			elm$core$Maybe$withDefault,
			0,
			A2(elm$core$Maybe$map, elm$core$String$length, maybeBackslashes));
		return elm$core$Maybe$Just(
			{
				index: regMatch.index + backslashesLength,
				length: 1,
				meaning: pablohirafuji$elm_markdown$Markdown$InlineParser$RightAngleBracket(
					!pablohirafuji$elm_markdown$Markdown$Helpers$isEven(backslashesLength))
			});
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$findAngleBracketRTokens = function (str) {
	return A2(
		elm$core$List$filterMap,
		pablohirafuji$elm_markdown$Markdown$InlineParser$regMatchToAngleBracketRToken,
		A2(elm$regex$Regex$find, pablohirafuji$elm_markdown$Markdown$InlineParser$angleBracketRTokenRegex, str));
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$asteriskEmphasisTokenRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('(\\\\*)([^*])?(\\*+)([^*])?'));
var pablohirafuji$elm_markdown$Markdown$InlineParser$EmphasisToken = F2(
	function (a, b) {
		return {$: 'EmphasisToken', a: a, b: b};
	});
var pablohirafuji$elm_markdown$Markdown$InlineParser$punctuationRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('[!-#%-\\*,-/:;\\?@\\[-\\]_\\{\\}]'));
var pablohirafuji$elm_markdown$Markdown$InlineParser$containPunctuation = elm$regex$Regex$contains(pablohirafuji$elm_markdown$Markdown$InlineParser$punctuationRegex);
var pablohirafuji$elm_markdown$Markdown$InlineParser$spaceRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('\\s'));
var pablohirafuji$elm_markdown$Markdown$InlineParser$containSpace = elm$regex$Regex$contains(pablohirafuji$elm_markdown$Markdown$InlineParser$spaceRegex);
var pablohirafuji$elm_markdown$Markdown$InlineParser$charFringeRank = function (_char) {
	var string = elm$core$String$fromChar(_char);
	return pablohirafuji$elm_markdown$Markdown$InlineParser$containSpace(string) ? 0 : (pablohirafuji$elm_markdown$Markdown$InlineParser$containPunctuation(string) ? 1 : 2);
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$maybeCharFringeRank = function (maybeChar) {
	return A2(
		elm$core$Maybe$withDefault,
		0,
		A2(elm$core$Maybe$map, pablohirafuji$elm_markdown$Markdown$InlineParser$charFringeRank, maybeChar));
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$getFringeRank = A2(
	elm$core$Basics$composeR,
	elm$core$Maybe$map(
		A2(
			elm$core$Basics$composeR,
			elm$core$String$uncons,
			A2(
				elm$core$Basics$composeR,
				elm$core$Maybe$map(elm$core$Tuple$first),
				pablohirafuji$elm_markdown$Markdown$InlineParser$maybeCharFringeRank))),
	elm$core$Maybe$withDefault(0));
var pablohirafuji$elm_markdown$Markdown$InlineParser$regMatchToEmphasisToken = F3(
	function (_char, rawText, regMatch) {
		var _n0 = regMatch.submatches;
		if ((((_n0.b && _n0.b.b) && _n0.b.b.b) && (_n0.b.b.a.$ === 'Just')) && _n0.b.b.b.b) {
			var maybeBackslashes = _n0.a;
			var _n1 = _n0.b;
			var maybeLeftFringe = _n1.a;
			var _n2 = _n1.b;
			var delimiter = _n2.a.a;
			var _n3 = _n2.b;
			var maybeRightFringe = _n3.a;
			var leftFringeLength = A2(
				elm$core$Maybe$withDefault,
				0,
				A2(elm$core$Maybe$map, elm$core$String$length, maybeLeftFringe));
			var mLeftFringe = (regMatch.index && (!leftFringeLength)) ? elm$core$Maybe$Just(
				A3(elm$core$String$slice, regMatch.index - 1, regMatch.index, rawText)) : maybeLeftFringe;
			var backslashesLength = A2(
				elm$core$Maybe$withDefault,
				0,
				A2(elm$core$Maybe$map, elm$core$String$length, maybeBackslashes));
			var isEscaped = ((!pablohirafuji$elm_markdown$Markdown$Helpers$isEven(backslashesLength)) && (!leftFringeLength)) || _Utils_eq(
				mLeftFringe,
				elm$core$Maybe$Just('\\'));
			var delimiterLength = isEscaped ? (elm$core$String$length(delimiter) - 1) : elm$core$String$length(delimiter);
			var fringeRank = _Utils_Tuple2(
				isEscaped ? 1 : pablohirafuji$elm_markdown$Markdown$InlineParser$getFringeRank(mLeftFringe),
				pablohirafuji$elm_markdown$Markdown$InlineParser$getFringeRank(maybeRightFringe));
			var index = ((regMatch.index + backslashesLength) + leftFringeLength) + (isEscaped ? 1 : 0);
			return ((delimiterLength <= 0) || (_Utils_eq(
				_char,
				_Utils_chr('_')) && _Utils_eq(
				fringeRank,
				_Utils_Tuple2(2, 2)))) ? elm$core$Maybe$Nothing : elm$core$Maybe$Just(
				{
					index: index,
					length: delimiterLength,
					meaning: A2(pablohirafuji$elm_markdown$Markdown$InlineParser$EmphasisToken, _char, fringeRank)
				});
		} else {
			return elm$core$Maybe$Nothing;
		}
	});
var pablohirafuji$elm_markdown$Markdown$InlineParser$findAsteriskEmphasisTokens = function (str) {
	return A2(
		elm$core$List$filterMap,
		A2(
			pablohirafuji$elm_markdown$Markdown$InlineParser$regMatchToEmphasisToken,
			_Utils_chr('*'),
			str),
		A2(elm$regex$Regex$find, pablohirafuji$elm_markdown$Markdown$InlineParser$asteriskEmphasisTokenRegex, str));
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$codeTokenRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('(\\\\*)(\\`+)'));
var pablohirafuji$elm_markdown$Markdown$InlineParser$CodeToken = function (a) {
	return {$: 'CodeToken', a: a};
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$regMatchToCodeToken = function (regMatch) {
	var _n0 = regMatch.submatches;
	if ((_n0.b && _n0.b.b) && (_n0.b.a.$ === 'Just')) {
		var maybeBackslashes = _n0.a;
		var _n1 = _n0.b;
		var backtick = _n1.a.a;
		var backslashesLength = A2(
			elm$core$Maybe$withDefault,
			0,
			A2(elm$core$Maybe$map, elm$core$String$length, maybeBackslashes));
		return elm$core$Maybe$Just(
			{
				index: regMatch.index + backslashesLength,
				length: elm$core$String$length(backtick),
				meaning: pablohirafuji$elm_markdown$Markdown$InlineParser$CodeToken(
					!pablohirafuji$elm_markdown$Markdown$Helpers$isEven(backslashesLength))
			});
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$findCodeTokens = function (str) {
	return A2(
		elm$core$List$filterMap,
		pablohirafuji$elm_markdown$Markdown$InlineParser$regMatchToCodeToken,
		A2(elm$regex$Regex$find, pablohirafuji$elm_markdown$Markdown$InlineParser$codeTokenRegex, str));
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$hardBreakTokenRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('(?:(\\\\+)|( {2,}))\\n'));
var pablohirafuji$elm_markdown$Markdown$InlineParser$HardLineBreakToken = {$: 'HardLineBreakToken'};
var pablohirafuji$elm_markdown$Markdown$InlineParser$regMatchToHardBreakToken = function (regMatch) {
	var _n0 = regMatch.submatches;
	_n0$2:
	while (true) {
		if (_n0.b) {
			if (_n0.a.$ === 'Just') {
				var backslashes = _n0.a.a;
				var backslashesLength = elm$core$String$length(backslashes);
				return (!pablohirafuji$elm_markdown$Markdown$Helpers$isEven(backslashesLength)) ? elm$core$Maybe$Just(
					{index: (regMatch.index + backslashesLength) - 1, length: 2, meaning: pablohirafuji$elm_markdown$Markdown$InlineParser$HardLineBreakToken}) : elm$core$Maybe$Nothing;
			} else {
				if (_n0.b.b && (_n0.b.a.$ === 'Just')) {
					var _n1 = _n0.b;
					return elm$core$Maybe$Just(
						{
							index: regMatch.index,
							length: elm$core$String$length(regMatch.match),
							meaning: pablohirafuji$elm_markdown$Markdown$InlineParser$HardLineBreakToken
						});
				} else {
					break _n0$2;
				}
			}
		} else {
			break _n0$2;
		}
	}
	return elm$core$Maybe$Nothing;
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$regMatchToSoftHardBreakToken = function (regMatch) {
	var _n0 = regMatch.submatches;
	_n0$2:
	while (true) {
		if (_n0.b) {
			if (_n0.a.$ === 'Just') {
				var backslashes = _n0.a.a;
				var backslashesLength = elm$core$String$length(backslashes);
				return pablohirafuji$elm_markdown$Markdown$Helpers$isEven(backslashesLength) ? elm$core$Maybe$Just(
					{index: regMatch.index + backslashesLength, length: 1, meaning: pablohirafuji$elm_markdown$Markdown$InlineParser$HardLineBreakToken}) : elm$core$Maybe$Just(
					{index: (regMatch.index + backslashesLength) - 1, length: 2, meaning: pablohirafuji$elm_markdown$Markdown$InlineParser$HardLineBreakToken});
			} else {
				if (_n0.b.b) {
					var _n1 = _n0.b;
					var maybeSpaces = _n1.a;
					return elm$core$Maybe$Just(
						{
							index: regMatch.index,
							length: elm$core$String$length(regMatch.match),
							meaning: pablohirafuji$elm_markdown$Markdown$InlineParser$HardLineBreakToken
						});
				} else {
					break _n0$2;
				}
			}
		} else {
			break _n0$2;
		}
	}
	return elm$core$Maybe$Nothing;
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$softAsHardLineBreakTokenRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('(?:(\\\\+)|( *))\\n'));
var pablohirafuji$elm_markdown$Markdown$InlineParser$findHardBreakTokens = F2(
	function (softAsHardLineBreak, str) {
		return softAsHardLineBreak ? A2(
			elm$core$List$filterMap,
			pablohirafuji$elm_markdown$Markdown$InlineParser$regMatchToSoftHardBreakToken,
			A2(elm$regex$Regex$find, pablohirafuji$elm_markdown$Markdown$InlineParser$softAsHardLineBreakTokenRegex, str)) : A2(
			elm$core$List$filterMap,
			pablohirafuji$elm_markdown$Markdown$InlineParser$regMatchToHardBreakToken,
			A2(elm$regex$Regex$find, pablohirafuji$elm_markdown$Markdown$InlineParser$hardBreakTokenRegex, str));
	});
var pablohirafuji$elm_markdown$Markdown$InlineParser$linkImageCloseTokenRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('(\\\\*)(\\])'));
var pablohirafuji$elm_markdown$Markdown$InlineParser$regMatchToLinkImageCloseToken = function (regMatch) {
	var _n0 = regMatch.submatches;
	if ((_n0.b && _n0.b.b) && (_n0.b.a.$ === 'Just')) {
		var maybeBackslashes = _n0.a;
		var _n1 = _n0.b;
		var delimiter = _n1.a.a;
		var backslashesLength = A2(
			elm$core$Maybe$withDefault,
			0,
			A2(elm$core$Maybe$map, elm$core$String$length, maybeBackslashes));
		return pablohirafuji$elm_markdown$Markdown$Helpers$isEven(backslashesLength) ? elm$core$Maybe$Just(
			{
				index: regMatch.index + backslashesLength,
				length: 1,
				meaning: pablohirafuji$elm_markdown$Markdown$InlineParser$CharToken(
					_Utils_chr(']'))
			}) : elm$core$Maybe$Nothing;
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$findLinkImageCloseTokens = function (str) {
	return A2(
		elm$core$List$filterMap,
		pablohirafuji$elm_markdown$Markdown$InlineParser$regMatchToLinkImageCloseToken,
		A2(elm$regex$Regex$find, pablohirafuji$elm_markdown$Markdown$InlineParser$linkImageCloseTokenRegex, str));
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$linkImageOpenTokenRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('(\\\\*)(\\!)?(\\[)'));
var pablohirafuji$elm_markdown$Markdown$InlineParser$ImageOpenToken = {$: 'ImageOpenToken'};
var pablohirafuji$elm_markdown$Markdown$InlineParser$LinkOpenToken = function (a) {
	return {$: 'LinkOpenToken', a: a};
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$regMatchToLinkImageOpenToken = function (regMatch) {
	var _n0 = regMatch.submatches;
	if (((_n0.b && _n0.b.b) && _n0.b.b.b) && (_n0.b.b.a.$ === 'Just')) {
		var maybeBackslashes = _n0.a;
		var _n1 = _n0.b;
		var maybeImageOpen = _n1.a;
		var _n2 = _n1.b;
		var delimiter = _n2.a.a;
		var backslashesLength = A2(
			elm$core$Maybe$withDefault,
			0,
			A2(elm$core$Maybe$map, elm$core$String$length, maybeBackslashes));
		var isEscaped = !pablohirafuji$elm_markdown$Markdown$Helpers$isEven(backslashesLength);
		var index = (regMatch.index + backslashesLength) + ((isEscaped && _Utils_eq(
			maybeImageOpen,
			elm$core$Maybe$Just('!'))) ? 1 : 0);
		var meaning = isEscaped ? A2(
			elm$core$Maybe$map,
			function (_n3) {
				return pablohirafuji$elm_markdown$Markdown$InlineParser$LinkOpenToken(true);
			},
			maybeImageOpen) : elm$core$Maybe$Just(
			A2(
				elm$core$Maybe$withDefault,
				pablohirafuji$elm_markdown$Markdown$InlineParser$LinkOpenToken(true),
				A2(
					elm$core$Maybe$map,
					function (_n4) {
						return pablohirafuji$elm_markdown$Markdown$InlineParser$ImageOpenToken;
					},
					maybeImageOpen)));
		var length = _Utils_eq(
			meaning,
			elm$core$Maybe$Just(pablohirafuji$elm_markdown$Markdown$InlineParser$ImageOpenToken)) ? 2 : 1;
		var toModel = function (m) {
			return {index: index, length: length, meaning: m};
		};
		return A2(elm$core$Maybe$map, toModel, meaning);
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$findLinkImageOpenTokens = function (str) {
	return A2(
		elm$core$List$filterMap,
		pablohirafuji$elm_markdown$Markdown$InlineParser$regMatchToLinkImageOpenToken,
		A2(elm$regex$Regex$find, pablohirafuji$elm_markdown$Markdown$InlineParser$linkImageOpenTokenRegex, str));
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$underlineEmphasisTokenRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('(\\\\*)([^_])?(\\_+)([^_])?'));
var pablohirafuji$elm_markdown$Markdown$InlineParser$findUnderlineEmphasisTokens = function (str) {
	return A2(
		elm$core$List$filterMap,
		A2(
			pablohirafuji$elm_markdown$Markdown$InlineParser$regMatchToEmphasisToken,
			_Utils_chr('_'),
			str),
		A2(elm$regex$Regex$find, pablohirafuji$elm_markdown$Markdown$InlineParser$underlineEmphasisTokenRegex, str));
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$tokenize = function (model) {
	return _Utils_update(
		model,
		{
			tokens: A2(
				elm$core$List$sortBy,
				function ($) {
					return $.index;
				},
				_Utils_ap(
					pablohirafuji$elm_markdown$Markdown$InlineParser$findAngleBracketRTokens(model.rawText),
					_Utils_ap(
						pablohirafuji$elm_markdown$Markdown$InlineParser$findAngleBracketLTokens(model.rawText),
						_Utils_ap(
							A2(pablohirafuji$elm_markdown$Markdown$InlineParser$findHardBreakTokens, model.options.softAsHardLineBreak, model.rawText),
							_Utils_ap(
								pablohirafuji$elm_markdown$Markdown$InlineParser$findLinkImageCloseTokens(model.rawText),
								_Utils_ap(
									pablohirafuji$elm_markdown$Markdown$InlineParser$findLinkImageOpenTokens(model.rawText),
									_Utils_ap(
										pablohirafuji$elm_markdown$Markdown$InlineParser$findUnderlineEmphasisTokens(model.rawText),
										_Utils_ap(
											pablohirafuji$elm_markdown$Markdown$InlineParser$findAsteriskEmphasisTokens(model.rawText),
											pablohirafuji$elm_markdown$Markdown$InlineParser$findCodeTokens(model.rawText)))))))))
		});
};
var elm$core$Result$toMaybe = function (result) {
	if (result.$ === 'Ok') {
		var v = result.a;
		return elm$core$Maybe$Just(v);
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var pablohirafuji$elm_markdown$Markdown$Helpers$whiteSpaceChars = ' \\t\\f\\v\\r\\n';
var pablohirafuji$elm_markdown$Markdown$Helpers$whitespacesRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('[' + (pablohirafuji$elm_markdown$Markdown$Helpers$whiteSpaceChars + ']+')));
var pablohirafuji$elm_markdown$Markdown$Helpers$cleanWhitespaces = A2(
	elm$core$Basics$composeR,
	elm$core$String$trim,
	A2(
		elm$regex$Regex$replace,
		pablohirafuji$elm_markdown$Markdown$Helpers$whitespacesRegex,
		function (_n0) {
			return ' ';
		}));
var pablohirafuji$elm_markdown$Markdown$InlineParser$CodeType = {$: 'CodeType'};
var pablohirafuji$elm_markdown$Markdown$InlineParser$EmphasisType = function (a) {
	return {$: 'EmphasisType', a: a};
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$HtmlType = function (a) {
	return {$: 'HtmlType', a: a};
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$ImageType = function (a) {
	return {$: 'ImageType', a: a};
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$LinkType = function (a) {
	return {$: 'LinkType', a: a};
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$addMatch = F2(
	function (model, match) {
		return _Utils_update(
			model,
			{
				matches: A2(elm$core$List$cons, match, model.matches)
			});
	});
var pablohirafuji$elm_markdown$Markdown$InlineParser$addToken = F2(
	function (model, token) {
		return _Utils_update(
			model,
			{
				tokens: A2(elm$core$List$cons, token, model.tokens)
			});
	});
var pablohirafuji$elm_markdown$Markdown$InlineParser$applyTTM = F2(
	function (finderFunction, model) {
		return finderFunction(
			_Utils_Tuple2(
				model.tokens,
				_Utils_update(
					model,
					{tokens: _List_Nil})));
	});
var pablohirafuji$elm_markdown$Markdown$InlineParser$AutolinkType = function (a) {
	return {$: 'AutolinkType', a: a};
};
var elm$url$Url$percentDecode = _Url_percentDecode;
var elm$url$Url$percentEncode = _Url_percentEncode;
var pablohirafuji$elm_markdown$Markdown$InlineParser$decodeUrlRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('%(?:3B|2C|2F|3F|3A|40|26|3D|2B|24|23|25)'));
var pablohirafuji$elm_markdown$Markdown$InlineParser$encodeUrl = A2(
	elm$core$Basics$composeR,
	elm$url$Url$percentEncode,
	A2(
		elm$regex$Regex$replace,
		pablohirafuji$elm_markdown$Markdown$InlineParser$decodeUrlRegex,
		function (match) {
			return A2(
				elm$core$Maybe$withDefault,
				match.match,
				elm$url$Url$percentDecode(match.match));
		}));
var pablohirafuji$elm_markdown$Markdown$InlineParser$urlRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('^([A-Za-z][A-Za-z0-9.+\\-]{1,31}:[^<>\\x00-\\x20]*)$'));
var pablohirafuji$elm_markdown$Markdown$InlineParser$autolinkToMatch = function (_n0) {
	var match = _n0.a;
	return A2(elm$regex$Regex$contains, pablohirafuji$elm_markdown$Markdown$InlineParser$urlRegex, match.text) ? elm$core$Result$Ok(
		pablohirafuji$elm_markdown$Markdown$InlineParser$Match(
			_Utils_update(
				match,
				{
					type_: pablohirafuji$elm_markdown$Markdown$InlineParser$AutolinkType(
						_Utils_Tuple2(
							match.text,
							pablohirafuji$elm_markdown$Markdown$InlineParser$encodeUrl(match.text)))
				}))) : elm$core$Result$Err(
		pablohirafuji$elm_markdown$Markdown$InlineParser$Match(match));
};
var pablohirafuji$elm_markdown$Markdown$Helpers$titleRegex = '(?:[' + (pablohirafuji$elm_markdown$Markdown$Helpers$whiteSpaceChars + (']+' + ('(?:\'([^\'\\\\]*(?:\\\\.[^\'\\\\]*)*)\'|' + ('\"([^\"\\\\]*(?:\\\\.[^\"\\\\]*)*)\"|' + '\\(([^\\)\\\\]*(?:\\\\.[^\\)\\\\]*)*)\\)))?'))));
var pablohirafuji$elm_markdown$Markdown$InlineParser$hrefRegex = '(?:<([^<>' + (pablohirafuji$elm_markdown$Markdown$Helpers$whiteSpaceChars + (']*)>|([^' + (pablohirafuji$elm_markdown$Markdown$Helpers$whiteSpaceChars + ('\\(\\)\\\\]*(?:\\\\.[^' + (pablohirafuji$elm_markdown$Markdown$Helpers$whiteSpaceChars + '\\(\\)\\\\]*)*))')))));
var pablohirafuji$elm_markdown$Markdown$InlineParser$inlineLinkTypeOrImageTypeRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('^\\(\\s*' + (pablohirafuji$elm_markdown$Markdown$InlineParser$hrefRegex + (pablohirafuji$elm_markdown$Markdown$Helpers$titleRegex + '\\s*\\)'))));
var pablohirafuji$elm_markdown$Markdown$Helpers$returnFirstJust = function (maybes) {
	var process = F2(
		function (a, maybeFound) {
			if (maybeFound.$ === 'Just') {
				var found = maybeFound.a;
				return elm$core$Maybe$Just(found);
			} else {
				return a;
			}
		});
	return A3(elm$core$List$foldl, process, elm$core$Maybe$Nothing, maybes);
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$prepareUrlAndTitle = function (_n0) {
	var rawUrl = _n0.a;
	var maybeTitle = _n0.b;
	return _Utils_Tuple2(
		pablohirafuji$elm_markdown$Markdown$InlineParser$encodeUrl(
			pablohirafuji$elm_markdown$Markdown$Helpers$formatStr(rawUrl)),
		A2(elm$core$Maybe$map, pablohirafuji$elm_markdown$Markdown$Helpers$formatStr, maybeTitle));
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$inlineLinkTypeOrImageTypeRegexToMatch = F3(
	function (matchModel, model, regexMatch) {
		var _n0 = regexMatch.submatches;
		if ((((_n0.b && _n0.b.b) && _n0.b.b.b) && _n0.b.b.b.b) && _n0.b.b.b.b.b) {
			var maybeRawUrlAngleBrackets = _n0.a;
			var _n1 = _n0.b;
			var maybeRawUrlWithoutBrackets = _n1.a;
			var _n2 = _n1.b;
			var maybeTitleSingleQuotes = _n2.a;
			var _n3 = _n2.b;
			var maybeTitleDoubleQuotes = _n3.a;
			var _n4 = _n3.b;
			var maybeTitleParenthesis = _n4.a;
			var maybeTitle = pablohirafuji$elm_markdown$Markdown$Helpers$returnFirstJust(
				_List_fromArray(
					[maybeTitleSingleQuotes, maybeTitleDoubleQuotes, maybeTitleParenthesis]));
			var toMatch = function (rawUrl) {
				return pablohirafuji$elm_markdown$Markdown$InlineParser$Match(
					_Utils_update(
						matchModel,
						{
							end: matchModel.end + elm$core$String$length(regexMatch.match),
							type_: function () {
								var _n5 = matchModel.type_;
								if (_n5.$ === 'ImageType') {
									return pablohirafuji$elm_markdown$Markdown$InlineParser$ImageType;
								} else {
									return pablohirafuji$elm_markdown$Markdown$InlineParser$LinkType;
								}
							}()(
								pablohirafuji$elm_markdown$Markdown$InlineParser$prepareUrlAndTitle(
									_Utils_Tuple2(rawUrl, maybeTitle)))
						}));
			};
			var maybeRawUrl = pablohirafuji$elm_markdown$Markdown$Helpers$returnFirstJust(
				_List_fromArray(
					[maybeRawUrlAngleBrackets, maybeRawUrlWithoutBrackets]));
			return elm$core$Maybe$Just(
				toMatch(
					A2(elm$core$Maybe$withDefault, '', maybeRawUrl)));
		} else {
			return elm$core$Maybe$Nothing;
		}
	});
var pablohirafuji$elm_markdown$Markdown$InlineParser$checkForInlineLinkTypeOrImageType = function (_n0) {
	var remainText = _n0.a;
	var tempMatch = _n0.b.a;
	var model = _n0.c;
	return A2(
		elm$core$Result$fromMaybe,
		_Utils_Tuple3(
			remainText,
			pablohirafuji$elm_markdown$Markdown$InlineParser$Match(tempMatch),
			model),
		A2(
			elm$core$Maybe$map,
			pablohirafuji$elm_markdown$Markdown$InlineParser$addMatch(model),
			A2(
				elm$core$Maybe$andThen,
				A2(pablohirafuji$elm_markdown$Markdown$InlineParser$inlineLinkTypeOrImageTypeRegexToMatch, tempMatch, model),
				elm$core$List$head(
					A3(elm$regex$Regex$findAtMost, 1, pablohirafuji$elm_markdown$Markdown$InlineParser$inlineLinkTypeOrImageTypeRegex, remainText)))));
};
var pablohirafuji$elm_markdown$Markdown$Helpers$insideSquareBracketRegex = '[^\\[\\]\\\\]*(?:\\\\.[^\\[\\]\\\\]*)*';
var pablohirafuji$elm_markdown$Markdown$InlineParser$refLabelRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('^\\[\\s*(' + (pablohirafuji$elm_markdown$Markdown$Helpers$insideSquareBracketRegex + ')\\s*\\]')));
var pablohirafuji$elm_markdown$Markdown$Helpers$prepareRefLabel = A2(elm$core$Basics$composeR, pablohirafuji$elm_markdown$Markdown$Helpers$cleanWhitespaces, elm$core$String$toLower);
var pablohirafuji$elm_markdown$Markdown$InlineParser$refRegexToMatch = F3(
	function (matchModel, model, maybeRegexMatch) {
		var regexMatchLength = A2(
			elm$core$Maybe$withDefault,
			0,
			A2(
				elm$core$Maybe$map,
				A2(
					elm$core$Basics$composeR,
					function ($) {
						return $.match;
					},
					elm$core$String$length),
				maybeRegexMatch));
		var toMatch = function (urlTitle) {
			return pablohirafuji$elm_markdown$Markdown$InlineParser$Match(
				_Utils_update(
					matchModel,
					{
						end: matchModel.end + regexMatchLength,
						type_: function () {
							var _n0 = matchModel.type_;
							if (_n0.$ === 'ImageType') {
								return pablohirafuji$elm_markdown$Markdown$InlineParser$ImageType;
							} else {
								return pablohirafuji$elm_markdown$Markdown$InlineParser$LinkType;
							}
						}()(
							pablohirafuji$elm_markdown$Markdown$InlineParser$prepareUrlAndTitle(urlTitle))
					}));
		};
		var refLabel = function (str) {
			return elm$core$String$isEmpty(str) ? matchModel.text : str;
		}(
			A2(
				elm$core$Maybe$withDefault,
				matchModel.text,
				A2(
					elm$core$Maybe$withDefault,
					elm$core$Maybe$Nothing,
					A2(
						elm$core$Maybe$withDefault,
						elm$core$Maybe$Nothing,
						A2(
							elm$core$Maybe$map,
							A2(
								elm$core$Basics$composeR,
								function ($) {
									return $.submatches;
								},
								elm$core$List$head),
							maybeRegexMatch)))));
		var maybeRefItem = A2(
			elm$core$Dict$get,
			pablohirafuji$elm_markdown$Markdown$Helpers$prepareRefLabel(refLabel),
			model.refs);
		return A2(elm$core$Maybe$map, toMatch, maybeRefItem);
	});
var pablohirafuji$elm_markdown$Markdown$InlineParser$checkForRefLinkTypeOrImageType = function (_n0) {
	var remainText = _n0.a;
	var tempMatch = _n0.b.a;
	var model = _n0.c;
	return A2(
		elm$core$Result$fromMaybe,
		_Utils_Tuple3(
			remainText,
			pablohirafuji$elm_markdown$Markdown$InlineParser$Match(tempMatch),
			model),
		A2(
			elm$core$Maybe$map,
			pablohirafuji$elm_markdown$Markdown$InlineParser$addMatch(model),
			A3(
				pablohirafuji$elm_markdown$Markdown$InlineParser$refRegexToMatch,
				tempMatch,
				model,
				elm$core$List$head(
					A3(elm$regex$Regex$findAtMost, 1, pablohirafuji$elm_markdown$Markdown$InlineParser$refLabelRegex, remainText)))));
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$checkParsedAheadOverlapping = function (parser) {
	var _n0 = parser.matches;
	if (!_n0.b) {
		return elm$core$Result$Err(_Utils_Tuple0);
	} else {
		var match = _n0.a.a;
		var remainMatches = _n0.b;
		var overlappingMatches = A2(
			elm$core$List$filter,
			function (_n1) {
				var testMatch = _n1.a;
				return (_Utils_cmp(match.end, testMatch.start) > 0) && (_Utils_cmp(match.end, testMatch.end) < 0);
			},
			remainMatches);
		return (elm$core$List$isEmpty(remainMatches) || elm$core$List$isEmpty(overlappingMatches)) ? elm$core$Result$Ok(parser) : elm$core$Result$Err(_Utils_Tuple0);
	}
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$emailRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('^([a-zA-Z0-9.!#$%&\'*+\\/=?^_`{|}~\\-]+@[a-zA-Z0-9](?:[a-zA-Z0-9\\-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9\\-]{0,61}[a-zA-Z0-9])?)*)$'));
var pablohirafuji$elm_markdown$Markdown$InlineParser$emailAutolinkTypeToMatch = function (_n0) {
	var match = _n0.a;
	return A2(elm$regex$Regex$contains, pablohirafuji$elm_markdown$Markdown$InlineParser$emailRegex, match.text) ? elm$core$Result$Ok(
		pablohirafuji$elm_markdown$Markdown$InlineParser$Match(
			_Utils_update(
				match,
				{
					type_: pablohirafuji$elm_markdown$Markdown$InlineParser$AutolinkType(
						_Utils_Tuple2(
							match.text,
							'mailto:' + pablohirafuji$elm_markdown$Markdown$InlineParser$encodeUrl(match.text)))
				}))) : elm$core$Result$Err(
		pablohirafuji$elm_markdown$Markdown$InlineParser$Match(match));
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$filterTokens = F2(
	function (filter, model) {
		return _Utils_update(
			model,
			{
				tokens: A2(elm$core$List$filter, filter, model.tokens)
			});
	});
var pablohirafuji$elm_markdown$Markdown$InlineParser$findToken = F2(
	function (isToken, tokens) {
		var search = F2(
			function (token, _n2) {
				var maybeToken = _n2.a;
				var innerTokens = _n2.b;
				var remainTokens = _n2.c;
				if (maybeToken.$ === 'Nothing') {
					return isToken(token) ? _Utils_Tuple3(
						elm$core$Maybe$Just(token),
						innerTokens,
						_List_Nil) : _Utils_Tuple3(
						elm$core$Maybe$Nothing,
						A2(elm$core$List$cons, token, innerTokens),
						_List_Nil);
				} else {
					return _Utils_Tuple3(
						maybeToken,
						innerTokens,
						A2(elm$core$List$cons, token, remainTokens));
				}
			});
		var _return = function (_n0) {
			var maybeToken = _n0.a;
			var innerTokens = _n0.b;
			var remainTokens = _n0.c;
			return A2(
				elm$core$Maybe$map,
				function (token) {
					return _Utils_Tuple3(
						token,
						elm$core$List$reverse(innerTokens),
						elm$core$List$reverse(remainTokens));
				},
				maybeToken);
		};
		return _return(
			A3(
				elm$core$List$foldl,
				search,
				_Utils_Tuple3(elm$core$Maybe$Nothing, _List_Nil, _List_Nil),
				tokens));
	});
var elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var pablohirafuji$elm_markdown$Markdown$InlineParser$HtmlModel = F2(
	function (tag, attributes) {
		return {attributes: attributes, tag: tag};
	});
var pablohirafuji$elm_markdown$Markdown$InlineParser$HtmlToken = F2(
	function (a, b) {
		return {$: 'HtmlToken', a: a, b: b};
	});
var pablohirafuji$elm_markdown$Markdown$InlineParser$attributesFromRegex = function (regexMatch) {
	var _n0 = regexMatch.submatches;
	_n0$2:
	while (true) {
		if (_n0.b && (_n0.a.$ === 'Just')) {
			if (_n0.a.a === '') {
				return elm$core$Maybe$Nothing;
			} else {
				if ((_n0.b.b && _n0.b.b.b) && _n0.b.b.b.b) {
					var name = _n0.a.a;
					var _n1 = _n0.b;
					var maybeDoubleQuotes = _n1.a;
					var _n2 = _n1.b;
					var maybeSingleQuotes = _n2.a;
					var _n3 = _n2.b;
					var maybeUnquoted = _n3.a;
					var maybeValue = pablohirafuji$elm_markdown$Markdown$Helpers$returnFirstJust(
						_List_fromArray(
							[maybeDoubleQuotes, maybeSingleQuotes, maybeUnquoted]));
					return elm$core$Maybe$Just(
						_Utils_Tuple2(name, maybeValue));
				} else {
					break _n0$2;
				}
			}
		} else {
			break _n0$2;
		}
	}
	return elm$core$Maybe$Nothing;
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$htmlAttributesRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('([a-zA-Z:_][a-zA-Z0-9\\-_.:]*)(?: ?= ?(?:\"([^\"]*)\"|\'([^\']*)\'|([^\\s\"\'=<>`]*)))?'));
var pablohirafuji$elm_markdown$Markdown$InlineParser$applyAttributesRegex = A2(
	elm$core$Basics$composeR,
	elm$regex$Regex$find(pablohirafuji$elm_markdown$Markdown$InlineParser$htmlAttributesRegex),
	elm$core$List$filterMap(pablohirafuji$elm_markdown$Markdown$InlineParser$attributesFromRegex));
var pablohirafuji$elm_markdown$Markdown$InlineParser$htmlFromRegex = F3(
	function (model, match, regexMatch) {
		var _n0 = regexMatch.submatches;
		if ((((_n0.b && _n0.b.b) && (_n0.b.a.$ === 'Just')) && _n0.b.b.b) && _n0.b.b.b.b) {
			var maybeClose = _n0.a;
			var _n1 = _n0.b;
			var tag = _n1.a.a;
			var _n2 = _n1.b;
			var maybeAttributes = _n2.a;
			var _n3 = _n2.b;
			var maybeSelfClosing = _n3.a;
			var updateModel = function (attrs) {
				return A2(
					pablohirafuji$elm_markdown$Markdown$InlineParser$addToken,
					model,
					{
						index: match.start,
						length: match.end - match.start,
						meaning: A2(
							pablohirafuji$elm_markdown$Markdown$InlineParser$HtmlToken,
							_Utils_eq(maybeClose, elm$core$Maybe$Nothing) && _Utils_eq(maybeSelfClosing, elm$core$Maybe$Nothing),
							A2(pablohirafuji$elm_markdown$Markdown$InlineParser$HtmlModel, tag, attrs))
					});
			};
			var filterAttributes = F2(
				function (attrs, allowed) {
					return A2(
						elm$core$List$filter,
						function (attr) {
							return A2(elm$core$List$member, attr.a, allowed);
						},
						attrs);
				});
			var attributes = A2(
				elm$core$Maybe$withDefault,
				_List_Nil,
				A2(elm$core$Maybe$map, pablohirafuji$elm_markdown$Markdown$InlineParser$applyAttributesRegex, maybeAttributes));
			var noAttributesInCloseTag = _Utils_eq(maybeClose, elm$core$Maybe$Nothing) || ((!_Utils_eq(maybeClose, elm$core$Maybe$Nothing)) && _Utils_eq(attributes, _List_Nil));
			var _n4 = model.options.rawHtml;
			switch (_n4.$) {
				case 'ParseUnsafe':
					return noAttributesInCloseTag ? elm$core$Maybe$Just(
						updateModel(attributes)) : elm$core$Maybe$Nothing;
				case 'Sanitize':
					var allowedHtmlElements = _n4.a.allowedHtmlElements;
					var allowedHtmlAttributes = _n4.a.allowedHtmlAttributes;
					return (A2(elm$core$List$member, tag, allowedHtmlElements) && noAttributesInCloseTag) ? elm$core$Maybe$Just(
						updateModel(
							A2(filterAttributes, attributes, allowedHtmlAttributes))) : elm$core$Maybe$Nothing;
				default:
					return elm$core$Maybe$Nothing;
			}
		} else {
			return elm$core$Maybe$Nothing;
		}
	});
var pablohirafuji$elm_markdown$Markdown$InlineParser$htmlRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('^(\\/)?([a-zA-Z][a-zA-Z0-9\\-]*)(?:\\s+([^<>]*?))?(\\/)?$'));
var pablohirafuji$elm_markdown$Markdown$InlineParser$htmlToToken = F2(
	function (model, _n0) {
		var match = _n0.a;
		var _n1 = model.options.rawHtml;
		if (_n1.$ === 'DontParse') {
			return elm$core$Maybe$Nothing;
		} else {
			return A2(
				elm$core$Maybe$andThen,
				A2(pablohirafuji$elm_markdown$Markdown$InlineParser$htmlFromRegex, model, match),
				elm$core$List$head(
					A3(elm$regex$Regex$findAtMost, 1, pablohirafuji$elm_markdown$Markdown$InlineParser$htmlRegex, match.text)));
		}
	});
var pablohirafuji$elm_markdown$Markdown$InlineParser$isCloseToken = F2(
	function (htmlModel, token) {
		var _n0 = token.meaning;
		if ((_n0.$ === 'HtmlToken') && (!_n0.a)) {
			var htmlModel_ = _n0.b;
			return _Utils_eq(htmlModel.tag, htmlModel_.tag);
		} else {
			return false;
		}
	});
var pablohirafuji$elm_markdown$Markdown$InlineParser$isCodeTokenPair = F2(
	function (closeToken, openToken) {
		var _n0 = openToken.meaning;
		if (_n0.$ === 'CodeToken') {
			var isEscaped = _n0.a;
			return isEscaped ? _Utils_eq(openToken.length - 1, closeToken.length) : _Utils_eq(openToken.length, closeToken.length);
		} else {
			return false;
		}
	});
var pablohirafuji$elm_markdown$Markdown$InlineParser$isLinkTypeOrImageOpenToken = function (token) {
	var _n0 = token.meaning;
	switch (_n0.$) {
		case 'LinkOpenToken':
			return true;
		case 'ImageOpenToken':
			return true;
		default:
			return false;
	}
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$isOpenEmphasisToken = F2(
	function (closeToken, openToken) {
		var _n0 = openToken.meaning;
		if (_n0.$ === 'EmphasisToken') {
			var openChar = _n0.a;
			var _n1 = _n0.b;
			var openLR = _n1.a;
			var openRR = _n1.b;
			var _n2 = closeToken.meaning;
			if (_n2.$ === 'EmphasisToken') {
				var closeChar = _n2.a;
				var _n3 = _n2.b;
				var closeLR = _n3.a;
				var closeRR = _n3.b;
				return _Utils_eq(openChar, closeChar) ? ((_Utils_eq(openLR, openRR) || _Utils_eq(closeLR, closeRR)) ? A2(elm$core$Basics$modBy, 3, closeToken.length + openToken.length) : true) : false;
			} else {
				return false;
			}
		} else {
			return false;
		}
	});
var pablohirafuji$elm_markdown$Markdown$InlineParser$voidHtmlTags = _List_fromArray(
	['area', 'base', 'br', 'col', 'embed', 'hr', 'img', 'input', 'keygen', 'link', 'meta', 'param', 'source', 'track', 'wbr']);
var pablohirafuji$elm_markdown$Markdown$InlineParser$isVoidTag = function (htmlModel) {
	return A2(elm$core$List$member, htmlModel.tag, pablohirafuji$elm_markdown$Markdown$InlineParser$voidHtmlTags);
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$HardLineBreakType = {$: 'HardLineBreakType'};
var pablohirafuji$elm_markdown$Markdown$InlineParser$SoftLineBreakToken = {$: 'SoftLineBreakToken'};
var pablohirafuji$elm_markdown$Markdown$InlineParser$reverseTokens = function (model) {
	return _Utils_update(
		model,
		{
			tokens: elm$core$List$reverse(model.tokens)
		});
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$tokenToMatch = F2(
	function (token, type_) {
		return pablohirafuji$elm_markdown$Markdown$InlineParser$Match(
			{end: token.index + token.length, matches: _List_Nil, start: token.index, text: '', textEnd: 0, textStart: 0, type_: type_});
	});
var pablohirafuji$elm_markdown$Markdown$InlineParser$lineBreakTTM = function (_n0) {
	lineBreakTTM:
	while (true) {
		var tokens = _n0.a;
		var model = _n0.b;
		if (!tokens.b) {
			return pablohirafuji$elm_markdown$Markdown$InlineParser$reverseTokens(model);
		} else {
			var token = tokens.a;
			var tokensTail = tokens.b;
			if (_Utils_eq(token.meaning, pablohirafuji$elm_markdown$Markdown$InlineParser$HardLineBreakToken) || (_Utils_eq(token.meaning, pablohirafuji$elm_markdown$Markdown$InlineParser$SoftLineBreakToken) && model.options.softAsHardLineBreak)) {
				return pablohirafuji$elm_markdown$Markdown$InlineParser$lineBreakTTM(
					function (b) {
						return _Utils_Tuple2(tokensTail, b);
					}(
						_Utils_update(
							model,
							{
								matches: A2(
									elm$core$List$cons,
									A2(pablohirafuji$elm_markdown$Markdown$InlineParser$tokenToMatch, token, pablohirafuji$elm_markdown$Markdown$InlineParser$HardLineBreakType),
									model.matches)
							})));
			} else {
				var $temp$_n0 = _Utils_Tuple2(
					tokensTail,
					A2(pablohirafuji$elm_markdown$Markdown$InlineParser$addToken, model, token));
				_n0 = $temp$_n0;
				continue lineBreakTTM;
			}
		}
	}
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$removeParsedAheadTokens = F2(
	function (tokensTail, parser) {
		var _n0 = parser.matches;
		if (!_n0.b) {
			return _Utils_Tuple2(tokensTail, parser);
		} else {
			var match = _n0.a.a;
			return _Utils_Tuple2(
				A2(
					elm$core$List$filter,
					function (token) {
						return _Utils_cmp(token.index, match.end) > -1;
					},
					tokensTail),
				parser);
		}
	});
var pablohirafuji$elm_markdown$Markdown$InlineParser$angleBracketsToMatch = F4(
	function (closeToken, isEscaped, model, _n24) {
		var openToken = _n24.a;
		var remainTokens = _n24.c;
		return function (result) {
			if (result.$ === 'Err') {
				var tempMatch = result.a;
				return (!isEscaped) ? A2(
					pablohirafuji$elm_markdown$Markdown$InlineParser$htmlToToken,
					_Utils_update(
						model,
						{tokens: remainTokens}),
					tempMatch) : elm$core$Result$toMaybe(result);
			} else {
				return elm$core$Result$toMaybe(result);
			}
		}(
			A2(
				elm$core$Result$map,
				function (newMatch) {
					return _Utils_update(
						model,
						{
							matches: A2(elm$core$List$cons, newMatch, model.matches),
							tokens: remainTokens
						});
				},
				A2(
					pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
					pablohirafuji$elm_markdown$Markdown$InlineParser$emailAutolinkTypeToMatch,
					pablohirafuji$elm_markdown$Markdown$InlineParser$autolinkToMatch(
						A6(
							pablohirafuji$elm_markdown$Markdown$InlineParser$tokenPairToMatch,
							model,
							function (s) {
								return s;
							},
							pablohirafuji$elm_markdown$Markdown$InlineParser$CodeType,
							openToken,
							closeToken,
							_List_Nil)))));
	});
var pablohirafuji$elm_markdown$Markdown$InlineParser$codeAutolinkTypeHtmlTagTTM = function (_n21) {
	codeAutolinkTypeHtmlTagTTM:
	while (true) {
		var tokens = _n21.a;
		var model = _n21.b;
		if (!tokens.b) {
			return pablohirafuji$elm_markdown$Markdown$InlineParser$reverseTokens(model);
		} else {
			var token = tokens.a;
			var tokensTail = tokens.b;
			var _n23 = token.meaning;
			switch (_n23.$) {
				case 'CodeToken':
					var isEscaped = _n23.a;
					return pablohirafuji$elm_markdown$Markdown$InlineParser$codeAutolinkTypeHtmlTagTTM(
						function (b) {
							return _Utils_Tuple2(tokensTail, b);
						}(
							A2(
								elm$core$Maybe$withDefault,
								A2(pablohirafuji$elm_markdown$Markdown$InlineParser$addToken, model, token),
								A2(
									elm$core$Maybe$map,
									A2(pablohirafuji$elm_markdown$Markdown$InlineParser$codeToMatch, token, model),
									A2(
										pablohirafuji$elm_markdown$Markdown$InlineParser$findToken,
										pablohirafuji$elm_markdown$Markdown$InlineParser$isCodeTokenPair(token),
										model.tokens)))));
				case 'RightAngleBracket':
					var isEscaped = _n23.a;
					return pablohirafuji$elm_markdown$Markdown$InlineParser$codeAutolinkTypeHtmlTagTTM(
						function (b) {
							return _Utils_Tuple2(tokensTail, b);
						}(
							A2(
								pablohirafuji$elm_markdown$Markdown$InlineParser$filterTokens,
								A2(
									elm$core$Basics$composeR,
									function ($) {
										return $.meaning;
									},
									elm$core$Basics$neq(
										pablohirafuji$elm_markdown$Markdown$InlineParser$CharToken(
											_Utils_chr('<')))),
								A2(
									elm$core$Maybe$withDefault,
									model,
									A2(
										elm$core$Maybe$andThen,
										A3(pablohirafuji$elm_markdown$Markdown$InlineParser$angleBracketsToMatch, token, isEscaped, model),
										A2(
											pablohirafuji$elm_markdown$Markdown$InlineParser$findToken,
											A2(
												elm$core$Basics$composeR,
												function ($) {
													return $.meaning;
												},
												elm$core$Basics$eq(
													pablohirafuji$elm_markdown$Markdown$InlineParser$CharToken(
														_Utils_chr('<')))),
											model.tokens))))));
				default:
					var $temp$_n21 = _Utils_Tuple2(
						tokensTail,
						A2(pablohirafuji$elm_markdown$Markdown$InlineParser$addToken, model, token));
					_n21 = $temp$_n21;
					continue codeAutolinkTypeHtmlTagTTM;
			}
		}
	}
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$codeToMatch = F3(
	function (closeToken, model, _n20) {
		var openToken = _n20.a;
		var remainTokens = _n20.c;
		var updtOpenToken = _Utils_eq(
			openToken.meaning,
			pablohirafuji$elm_markdown$Markdown$InlineParser$CodeToken(true)) ? _Utils_update(
			openToken,
			{index: openToken.index + 1, length: openToken.length - 1}) : openToken;
		return _Utils_update(
			model,
			{
				matches: A2(
					elm$core$List$cons,
					A6(pablohirafuji$elm_markdown$Markdown$InlineParser$tokenPairToMatch, model, pablohirafuji$elm_markdown$Markdown$Helpers$cleanWhitespaces, pablohirafuji$elm_markdown$Markdown$InlineParser$CodeType, updtOpenToken, closeToken, _List_Nil),
					model.matches),
				tokens: remainTokens
			});
	});
var pablohirafuji$elm_markdown$Markdown$InlineParser$emphasisTTM = function (_n16) {
	emphasisTTM:
	while (true) {
		var tokens = _n16.a;
		var model = _n16.b;
		if (!tokens.b) {
			return pablohirafuji$elm_markdown$Markdown$InlineParser$reverseTokens(model);
		} else {
			var token = tokens.a;
			var tokensTail = tokens.b;
			var _n18 = token.meaning;
			if (_n18.$ === 'EmphasisToken') {
				var _char = _n18.a;
				var _n19 = _n18.b;
				var leftRank = _n19.a;
				var rightRank = _n19.b;
				if (_Utils_eq(leftRank, rightRank)) {
					if (rightRank && ((!_Utils_eq(
						_char,
						_Utils_chr('_'))) || (rightRank === 1))) {
						return pablohirafuji$elm_markdown$Markdown$InlineParser$emphasisTTM(
							A2(
								elm$core$Maybe$withDefault,
								_Utils_Tuple2(
									tokensTail,
									A2(pablohirafuji$elm_markdown$Markdown$InlineParser$addToken, model, token)),
								A2(
									elm$core$Maybe$map,
									A3(pablohirafuji$elm_markdown$Markdown$InlineParser$emphasisToMatch, token, tokensTail, model),
									A2(
										pablohirafuji$elm_markdown$Markdown$InlineParser$findToken,
										pablohirafuji$elm_markdown$Markdown$InlineParser$isOpenEmphasisToken(token),
										model.tokens))));
					} else {
						var $temp$_n16 = _Utils_Tuple2(tokensTail, model);
						_n16 = $temp$_n16;
						continue emphasisTTM;
					}
				} else {
					if (_Utils_cmp(leftRank, rightRank) < 0) {
						var $temp$_n16 = _Utils_Tuple2(
							tokensTail,
							A2(pablohirafuji$elm_markdown$Markdown$InlineParser$addToken, model, token));
						_n16 = $temp$_n16;
						continue emphasisTTM;
					} else {
						return pablohirafuji$elm_markdown$Markdown$InlineParser$emphasisTTM(
							A2(
								elm$core$Maybe$withDefault,
								_Utils_Tuple2(tokensTail, model),
								A2(
									elm$core$Maybe$map,
									A3(pablohirafuji$elm_markdown$Markdown$InlineParser$emphasisToMatch, token, tokensTail, model),
									A2(
										pablohirafuji$elm_markdown$Markdown$InlineParser$findToken,
										pablohirafuji$elm_markdown$Markdown$InlineParser$isOpenEmphasisToken(token),
										model.tokens))));
					}
				}
			} else {
				var $temp$_n16 = _Utils_Tuple2(
					tokensTail,
					A2(pablohirafuji$elm_markdown$Markdown$InlineParser$addToken, model, token));
				_n16 = $temp$_n16;
				continue emphasisTTM;
			}
		}
	}
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$emphasisToMatch = F4(
	function (closeToken, tokensTail, model, _n15) {
		var openToken = _n15.a;
		var innerTokens = _n15.b;
		var remainTokens = _n15.c;
		var remainLength = openToken.length - closeToken.length;
		var updt = (!remainLength) ? {closeToken: closeToken, openToken: openToken, remainTokens: remainTokens, tokensTail: tokensTail} : ((remainLength > 0) ? {
			closeToken: closeToken,
			openToken: _Utils_update(
				openToken,
				{index: openToken.index + remainLength, length: closeToken.length}),
			remainTokens: A2(
				elm$core$List$cons,
				_Utils_update(
					openToken,
					{length: remainLength}),
				remainTokens),
			tokensTail: tokensTail
		} : {
			closeToken: _Utils_update(
				closeToken,
				{length: openToken.length}),
			openToken: openToken,
			remainTokens: remainTokens,
			tokensTail: A2(
				elm$core$List$cons,
				_Utils_update(
					closeToken,
					{index: closeToken.index + openToken.length, length: -remainLength}),
				tokensTail)
		});
		var match = A6(
			pablohirafuji$elm_markdown$Markdown$InlineParser$tokenPairToMatch,
			model,
			function (s) {
				return s;
			},
			pablohirafuji$elm_markdown$Markdown$InlineParser$EmphasisType(updt.openToken.length),
			updt.openToken,
			updt.closeToken,
			elm$core$List$reverse(innerTokens));
		return _Utils_Tuple2(
			updt.tokensTail,
			_Utils_update(
				model,
				{
					matches: A2(elm$core$List$cons, match, model.matches),
					tokens: updt.remainTokens
				}));
	});
var pablohirafuji$elm_markdown$Markdown$InlineParser$htmlElementTTM = function (_n12) {
	htmlElementTTM:
	while (true) {
		var tokens = _n12.a;
		var model = _n12.b;
		if (!tokens.b) {
			return pablohirafuji$elm_markdown$Markdown$InlineParser$reverseTokens(model);
		} else {
			var token = tokens.a;
			var tokensTail = tokens.b;
			var _n14 = token.meaning;
			if (_n14.$ === 'HtmlToken') {
				var isOpen = _n14.a;
				var htmlModel = _n14.b;
				return (pablohirafuji$elm_markdown$Markdown$InlineParser$isVoidTag(htmlModel) || (!isOpen)) ? pablohirafuji$elm_markdown$Markdown$InlineParser$htmlElementTTM(
					function (b) {
						return _Utils_Tuple2(tokensTail, b);
					}(
						A2(
							pablohirafuji$elm_markdown$Markdown$InlineParser$addMatch,
							model,
							A2(
								pablohirafuji$elm_markdown$Markdown$InlineParser$tokenToMatch,
								token,
								pablohirafuji$elm_markdown$Markdown$InlineParser$HtmlType(htmlModel))))) : pablohirafuji$elm_markdown$Markdown$InlineParser$htmlElementTTM(
					A2(
						elm$core$Maybe$withDefault,
						function (b) {
							return _Utils_Tuple2(tokensTail, b);
						}(
							A2(
								pablohirafuji$elm_markdown$Markdown$InlineParser$addMatch,
								model,
								A2(
									pablohirafuji$elm_markdown$Markdown$InlineParser$tokenToMatch,
									token,
									pablohirafuji$elm_markdown$Markdown$InlineParser$HtmlType(htmlModel)))),
						A2(
							elm$core$Maybe$map,
							A3(pablohirafuji$elm_markdown$Markdown$InlineParser$htmlElementToMatch, token, model, htmlModel),
							A2(
								pablohirafuji$elm_markdown$Markdown$InlineParser$findToken,
								pablohirafuji$elm_markdown$Markdown$InlineParser$isCloseToken(htmlModel),
								tokensTail))));
			} else {
				var $temp$_n12 = _Utils_Tuple2(
					tokensTail,
					A2(pablohirafuji$elm_markdown$Markdown$InlineParser$addToken, model, token));
				_n12 = $temp$_n12;
				continue htmlElementTTM;
			}
		}
	}
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$htmlElementToMatch = F4(
	function (openToken, model, htmlModel, _n11) {
		var closeToken = _n11.a;
		var innerTokens = _n11.b;
		var remainTokens = _n11.c;
		return _Utils_Tuple2(
			remainTokens,
			_Utils_update(
				model,
				{
					matches: A2(
						elm$core$List$cons,
						A6(
							pablohirafuji$elm_markdown$Markdown$InlineParser$tokenPairToMatch,
							model,
							function (s) {
								return s;
							},
							pablohirafuji$elm_markdown$Markdown$InlineParser$HtmlType(htmlModel),
							openToken,
							closeToken,
							innerTokens),
						model.matches)
				}));
	});
var pablohirafuji$elm_markdown$Markdown$InlineParser$linkImageTypeTTM = function (_n8) {
	linkImageTypeTTM:
	while (true) {
		var tokens = _n8.a;
		var model = _n8.b;
		if (!tokens.b) {
			return pablohirafuji$elm_markdown$Markdown$InlineParser$reverseTokens(model);
		} else {
			var token = tokens.a;
			var tokensTail = tokens.b;
			var _n10 = token.meaning;
			if ((_n10.$ === 'CharToken') && (']' === _n10.a.valueOf())) {
				return pablohirafuji$elm_markdown$Markdown$InlineParser$linkImageTypeTTM(
					A2(
						elm$core$Maybe$withDefault,
						_Utils_Tuple2(tokensTail, model),
						A2(
							elm$core$Maybe$andThen,
							A3(pablohirafuji$elm_markdown$Markdown$InlineParser$linkOrImageTypeToMatch, token, tokensTail, model),
							A2(pablohirafuji$elm_markdown$Markdown$InlineParser$findToken, pablohirafuji$elm_markdown$Markdown$InlineParser$isLinkTypeOrImageOpenToken, model.tokens))));
			} else {
				var $temp$_n8 = _Utils_Tuple2(
					tokensTail,
					A2(pablohirafuji$elm_markdown$Markdown$InlineParser$addToken, model, token));
				_n8 = $temp$_n8;
				continue linkImageTypeTTM;
			}
		}
	}
};
var pablohirafuji$elm_markdown$Markdown$InlineParser$linkOrImageTypeToMatch = F4(
	function (closeToken, tokensTail, model, _n1) {
		var openToken = _n1.a;
		var innerTokens = _n1.b;
		var remainTokens = _n1.c;
		var tempMatch = function (isLinkType) {
			return A6(
				pablohirafuji$elm_markdown$Markdown$InlineParser$tokenPairToMatch,
				model,
				function (s) {
					return s;
				},
				isLinkType ? pablohirafuji$elm_markdown$Markdown$InlineParser$LinkType(
					_Utils_Tuple2('', elm$core$Maybe$Nothing)) : pablohirafuji$elm_markdown$Markdown$InlineParser$ImageType(
					_Utils_Tuple2('', elm$core$Maybe$Nothing)),
				openToken,
				closeToken,
				elm$core$List$reverse(innerTokens));
		};
		var removeOpenToken = _Utils_Tuple2(
			tokensTail,
			_Utils_update(
				model,
				{
					tokens: _Utils_ap(innerTokens, remainTokens)
				}));
		var remainText = A2(elm$core$String$dropLeft, closeToken.index + 1, model.rawText);
		var linkOpenTokenToInactive = function (model_) {
			var process = function (token) {
				var _n7 = token.meaning;
				if (_n7.$ === 'LinkOpenToken') {
					return _Utils_update(
						token,
						{
							meaning: pablohirafuji$elm_markdown$Markdown$InlineParser$LinkOpenToken(false)
						});
				} else {
					return token;
				}
			};
			return _Utils_update(
				model_,
				{
					tokens: A2(elm$core$List$map, process, model_.tokens)
				});
		};
		var args = function (isLinkType) {
			return _Utils_Tuple3(
				remainText,
				tempMatch(isLinkType),
				_Utils_update(
					model,
					{tokens: remainTokens}));
		};
		var _n2 = openToken.meaning;
		switch (_n2.$) {
			case 'ImageOpenToken':
				return elm$core$Result$toMaybe(
					A2(
						pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
						function (_n4) {
							return elm$core$Result$Ok(removeOpenToken);
						},
						A2(
							elm$core$Result$map,
							pablohirafuji$elm_markdown$Markdown$InlineParser$removeParsedAheadTokens(tokensTail),
							A2(
								elm$core$Result$andThen,
								pablohirafuji$elm_markdown$Markdown$InlineParser$checkParsedAheadOverlapping,
								A2(
									elm$core$Result$mapError,
									function (_n3) {
										return _Utils_Tuple0;
									},
									A2(
										pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
										pablohirafuji$elm_markdown$Markdown$InlineParser$checkForRefLinkTypeOrImageType,
										pablohirafuji$elm_markdown$Markdown$InlineParser$checkForInlineLinkTypeOrImageType(
											args(false))))))));
			case 'LinkOpenToken':
				if (_n2.a) {
					return elm$core$Result$toMaybe(
						A2(
							pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
							function (_n6) {
								return elm$core$Result$Ok(removeOpenToken);
							},
							A2(
								elm$core$Result$map,
								pablohirafuji$elm_markdown$Markdown$InlineParser$removeParsedAheadTokens(tokensTail),
								A2(
									elm$core$Result$map,
									linkOpenTokenToInactive,
									A2(
										elm$core$Result$andThen,
										pablohirafuji$elm_markdown$Markdown$InlineParser$checkParsedAheadOverlapping,
										A2(
											elm$core$Result$mapError,
											function (_n5) {
												return _Utils_Tuple0;
											},
											A2(
												pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
												pablohirafuji$elm_markdown$Markdown$InlineParser$checkForRefLinkTypeOrImageType,
												pablohirafuji$elm_markdown$Markdown$InlineParser$checkForInlineLinkTypeOrImageType(
													args(true)))))))));
				} else {
					return elm$core$Maybe$Just(removeOpenToken);
				}
			default:
				return elm$core$Maybe$Nothing;
		}
	});
var pablohirafuji$elm_markdown$Markdown$InlineParser$tokenPairToMatch = F6(
	function (model, processText, type_, openToken, closeToken, innerTokens) {
		var textStart = openToken.index + openToken.length;
		var textEnd = closeToken.index;
		var start = openToken.index;
		var end = closeToken.index + closeToken.length;
		var match = {
			end: end,
			matches: _List_Nil,
			start: start,
			text: processText(
				A3(elm$core$String$slice, textStart, textEnd, model.rawText)),
			textEnd: textEnd,
			textStart: textStart,
			type_: type_
		};
		var matches = A2(
			elm$core$List$map,
			function (_n0) {
				var matchModel = _n0.a;
				return A2(pablohirafuji$elm_markdown$Markdown$InlineParser$prepareChildMatch, match, matchModel);
			},
			pablohirafuji$elm_markdown$Markdown$InlineParser$cyclic$tokensToMatches()(
				_Utils_update(
					model,
					{matches: _List_Nil, tokens: innerTokens})).matches);
		return pablohirafuji$elm_markdown$Markdown$InlineParser$Match(
			_Utils_update(
				match,
				{matches: matches}));
	});
function pablohirafuji$elm_markdown$Markdown$InlineParser$cyclic$tokensToMatches() {
	return A2(
		elm$core$Basics$composeR,
		pablohirafuji$elm_markdown$Markdown$InlineParser$applyTTM(pablohirafuji$elm_markdown$Markdown$InlineParser$codeAutolinkTypeHtmlTagTTM),
		A2(
			elm$core$Basics$composeR,
			pablohirafuji$elm_markdown$Markdown$InlineParser$applyTTM(pablohirafuji$elm_markdown$Markdown$InlineParser$htmlElementTTM),
			A2(
				elm$core$Basics$composeR,
				pablohirafuji$elm_markdown$Markdown$InlineParser$applyTTM(pablohirafuji$elm_markdown$Markdown$InlineParser$linkImageTypeTTM),
				A2(
					elm$core$Basics$composeR,
					pablohirafuji$elm_markdown$Markdown$InlineParser$applyTTM(pablohirafuji$elm_markdown$Markdown$InlineParser$emphasisTTM),
					pablohirafuji$elm_markdown$Markdown$InlineParser$applyTTM(pablohirafuji$elm_markdown$Markdown$InlineParser$lineBreakTTM)))));
}
try {
	var pablohirafuji$elm_markdown$Markdown$InlineParser$tokensToMatches = pablohirafuji$elm_markdown$Markdown$InlineParser$cyclic$tokensToMatches();
	pablohirafuji$elm_markdown$Markdown$InlineParser$cyclic$tokensToMatches = function () {
		return pablohirafuji$elm_markdown$Markdown$InlineParser$tokensToMatches;
	};
} catch ($) {
throw 'Some top-level definitions from `Markdown.InlineParser` are causing infinite recursion:\n\n  ┌─────┐\n  │    angleBracketsToMatch\n  │     ↓\n  │    tokensToMatches\n  │     ↓\n  │    codeAutolinkTypeHtmlTagTTM\n  │     ↓\n  │    codeToMatch\n  │     ↓\n  │    emphasisTTM\n  │     ↓\n  │    emphasisToMatch\n  │     ↓\n  │    htmlElementTTM\n  │     ↓\n  │    htmlElementToMatch\n  │     ↓\n  │    linkImageTypeTTM\n  │     ↓\n  │    linkOrImageTypeToMatch\n  │     ↓\n  │    tokenPairToMatch\n  └─────┘\n\nThese errors are very tricky, so read https://elm-lang.org/0.19.0/halting-problem to learn how to fix it!';}
var pablohirafuji$elm_markdown$Markdown$InlineParser$parse = F3(
	function (options, refs, rawText) {
		return pablohirafuji$elm_markdown$Markdown$InlineParser$matchesToInlines(
			pablohirafuji$elm_markdown$Markdown$InlineParser$parseText(
				pablohirafuji$elm_markdown$Markdown$InlineParser$organizeParserMatches(
					pablohirafuji$elm_markdown$Markdown$InlineParser$tokensToMatches(
						pablohirafuji$elm_markdown$Markdown$InlineParser$tokenize(
							A3(
								pablohirafuji$elm_markdown$Markdown$InlineParser$initParser,
								options,
								refs,
								elm$core$String$trim(rawText)))))).matches);
	});
var pablohirafuji$elm_markdown$Markdown$Block$parseInline = F4(
	function (maybeOptions, textAsParagraph, refs, block) {
		var options = A2(elm$core$Maybe$withDefault, pablohirafuji$elm_markdown$Markdown$Config$defaultOptions, maybeOptions);
		switch (block.$) {
			case 'Heading':
				var rawText = block.a;
				var lvl = block.b;
				return A3(
					pablohirafuji$elm_markdown$Markdown$Block$Heading,
					rawText,
					lvl,
					A3(pablohirafuji$elm_markdown$Markdown$InlineParser$parse, options, refs, rawText));
			case 'Paragraph':
				var rawText = block.a;
				var inlines = A3(pablohirafuji$elm_markdown$Markdown$InlineParser$parse, options, refs, rawText);
				if ((inlines.b && (inlines.a.$ === 'HtmlInline')) && (!inlines.b.b)) {
					var _n3 = inlines.a;
					return pablohirafuji$elm_markdown$Markdown$Block$PlainInlines(inlines);
				} else {
					return textAsParagraph ? A2(pablohirafuji$elm_markdown$Markdown$Block$Paragraph, rawText, inlines) : pablohirafuji$elm_markdown$Markdown$Block$PlainInlines(inlines);
				}
			case 'BlockQuote':
				var blocks = block.a;
				return pablohirafuji$elm_markdown$Markdown$Block$BlockQuote(
					A3(
						pablohirafuji$elm_markdown$Markdown$Block$parseInlines,
						maybeOptions,
						true,
						_Utils_Tuple2(refs, blocks)));
			case 'List':
				var model = block.a;
				var items = block.b;
				return A2(
					pablohirafuji$elm_markdown$Markdown$Block$List,
					model,
					function (a) {
						return A2(elm$core$List$map, a, items);
					}(
						A2(
							elm$core$Basics$composeL,
							A2(pablohirafuji$elm_markdown$Markdown$Block$parseInlines, maybeOptions, model.isLoose),
							function (b) {
								return _Utils_Tuple2(refs, b);
							})));
			case 'Custom':
				var customBlock = block.a;
				var blocks = block.b;
				return A2(
					pablohirafuji$elm_markdown$Markdown$Block$Custom,
					customBlock,
					A3(
						pablohirafuji$elm_markdown$Markdown$Block$parseInlines,
						maybeOptions,
						true,
						_Utils_Tuple2(refs, blocks)));
			default:
				return block;
		}
	});
var pablohirafuji$elm_markdown$Markdown$Block$parseInlines = F3(
	function (maybeOptions, textAsParagraph, _n0) {
		var refs = _n0.a;
		var blocks = _n0.b;
		return A2(
			elm$core$List$map,
			A3(pablohirafuji$elm_markdown$Markdown$Block$parseInline, maybeOptions, textAsParagraph, refs),
			blocks);
	});
var elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3(elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3(elm$core$Dict$foldl, elm$core$Dict$insert, t2, t1);
	});
var pablohirafuji$elm_markdown$Markdown$Block$dropRefString = F2(
	function (rawText, inlineMatch) {
		var strippedText = A2(elm$core$String$dropLeft, inlineMatch.matchLength, rawText);
		return A2(elm$regex$Regex$contains, pablohirafuji$elm_markdown$Markdown$Block$blankLineRegex, strippedText) ? elm$core$Maybe$Nothing : elm$core$Maybe$Just(strippedText);
	});
var pablohirafuji$elm_markdown$Markdown$Block$insertLinkMatch = F2(
	function (refs, linkMatch) {
		return A2(elm$core$Dict$member, linkMatch.inside, refs) ? refs : A3(
			elm$core$Dict$insert,
			linkMatch.inside,
			_Utils_Tuple2(linkMatch.url, linkMatch.maybeTitle),
			refs);
	});
var pablohirafuji$elm_markdown$Markdown$Block$extractUrlTitleRegex = function (regexMatch) {
	var _n0 = regexMatch.submatches;
	if ((((((_n0.b && (_n0.a.$ === 'Just')) && _n0.b.b) && _n0.b.b.b) && _n0.b.b.b.b) && _n0.b.b.b.b.b) && _n0.b.b.b.b.b.b) {
		var rawText = _n0.a.a;
		var _n1 = _n0.b;
		var maybeRawUrlAngleBrackets = _n1.a;
		var _n2 = _n1.b;
		var maybeRawUrlWithoutBrackets = _n2.a;
		var _n3 = _n2.b;
		var maybeTitleSingleQuotes = _n3.a;
		var _n4 = _n3.b;
		var maybeTitleDoubleQuotes = _n4.a;
		var _n5 = _n4.b;
		var maybeTitleParenthesis = _n5.a;
		var toReturn = function (rawUrl) {
			return {
				inside: rawText,
				matchLength: elm$core$String$length(regexMatch.match),
				maybeTitle: pablohirafuji$elm_markdown$Markdown$Helpers$returnFirstJust(
					_List_fromArray(
						[maybeTitleSingleQuotes, maybeTitleDoubleQuotes, maybeTitleParenthesis])),
				url: rawUrl
			};
		};
		var maybeRawUrl = pablohirafuji$elm_markdown$Markdown$Helpers$returnFirstJust(
			_List_fromArray(
				[maybeRawUrlAngleBrackets, maybeRawUrlWithoutBrackets]));
		return A2(elm$core$Maybe$map, toReturn, maybeRawUrl);
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var pablohirafuji$elm_markdown$Markdown$Block$hrefRegex = '\\s*(?:<([^<>\\s]*)>|([^\\s]*))';
var pablohirafuji$elm_markdown$Markdown$Block$refRegex = A2(
	elm$core$Maybe$withDefault,
	elm$regex$Regex$never,
	elm$regex$Regex$fromString('^\\s*\\[(' + (pablohirafuji$elm_markdown$Markdown$Helpers$insideSquareBracketRegex + (')\\]:' + (pablohirafuji$elm_markdown$Markdown$Block$hrefRegex + (pablohirafuji$elm_markdown$Markdown$Helpers$titleRegex + '\\s*(?![^\\n])'))))));
var pablohirafuji$elm_markdown$Markdown$Block$maybeLinkMatch = function (rawText) {
	return A2(
		elm$core$Maybe$andThen,
		function (linkMatch) {
			return ((linkMatch.url === '') || (linkMatch.inside === '')) ? elm$core$Maybe$Nothing : elm$core$Maybe$Just(linkMatch);
		},
		A2(
			elm$core$Maybe$map,
			function (linkMatch) {
				return _Utils_update(
					linkMatch,
					{
						inside: pablohirafuji$elm_markdown$Markdown$Helpers$prepareRefLabel(linkMatch.inside)
					});
			},
			A2(
				elm$core$Maybe$andThen,
				pablohirafuji$elm_markdown$Markdown$Block$extractUrlTitleRegex,
				elm$core$List$head(
					A3(elm$regex$Regex$findAtMost, 1, pablohirafuji$elm_markdown$Markdown$Block$refRegex, rawText)))));
};
var pablohirafuji$elm_markdown$Markdown$Block$parseReference = F2(
	function (refs, rawText) {
		parseReference:
		while (true) {
			var _n0 = pablohirafuji$elm_markdown$Markdown$Block$maybeLinkMatch(rawText);
			if (_n0.$ === 'Just') {
				var linkMatch = _n0.a;
				var updtRefs = A2(pablohirafuji$elm_markdown$Markdown$Block$insertLinkMatch, refs, linkMatch);
				var maybeStrippedText = A2(pablohirafuji$elm_markdown$Markdown$Block$dropRefString, rawText, linkMatch);
				if (maybeStrippedText.$ === 'Just') {
					var strippedText = maybeStrippedText.a;
					var $temp$refs = updtRefs,
						$temp$rawText = strippedText;
					refs = $temp$refs;
					rawText = $temp$rawText;
					continue parseReference;
				} else {
					return _Utils_Tuple2(updtRefs, elm$core$Maybe$Nothing);
				}
			} else {
				return _Utils_Tuple2(
					refs,
					elm$core$Maybe$Just(rawText));
			}
		}
	});
var pablohirafuji$elm_markdown$Markdown$Block$parseReferences = function (refs) {
	return A2(
		elm$core$List$foldl,
		pablohirafuji$elm_markdown$Markdown$Block$parseReferencesHelp,
		_Utils_Tuple2(refs, _List_Nil));
};
var pablohirafuji$elm_markdown$Markdown$Block$parseReferencesHelp = F2(
	function (block, _n0) {
		var refs = _n0.a;
		var parsedAST = _n0.b;
		switch (block.$) {
			case 'Paragraph':
				var rawText = block.a;
				var _n2 = A2(pablohirafuji$elm_markdown$Markdown$Block$parseReference, elm$core$Dict$empty, rawText);
				var paragraphRefs = _n2.a;
				var maybeUpdtText = _n2.b;
				var updtRefs = A2(elm$core$Dict$union, paragraphRefs, refs);
				if (maybeUpdtText.$ === 'Just') {
					var updtText = maybeUpdtText.a;
					return _Utils_Tuple2(
						updtRefs,
						A2(
							elm$core$List$cons,
							A2(pablohirafuji$elm_markdown$Markdown$Block$Paragraph, updtText, _List_Nil),
							parsedAST));
				} else {
					return _Utils_Tuple2(updtRefs, parsedAST);
				}
			case 'List':
				var model = block.a;
				var items = block.b;
				var _n4 = A3(
					elm$core$List$foldl,
					F2(
						function (item, _n5) {
							var refs__ = _n5.a;
							var parsedItems = _n5.b;
							return A2(
								elm$core$Tuple$mapSecond,
								function (a) {
									return A2(elm$core$List$cons, a, parsedItems);
								},
								A2(pablohirafuji$elm_markdown$Markdown$Block$parseReferences, refs__, item));
						}),
					_Utils_Tuple2(refs, _List_Nil),
					items);
				var updtRefs = _n4.a;
				var updtItems = _n4.b;
				return _Utils_Tuple2(
					updtRefs,
					A2(
						elm$core$List$cons,
						A2(pablohirafuji$elm_markdown$Markdown$Block$List, model, updtItems),
						parsedAST));
			case 'BlockQuote':
				var blocks = block.a;
				return A2(
					elm$core$Tuple$mapSecond,
					function (a) {
						return A2(elm$core$List$cons, a, parsedAST);
					},
					A2(
						elm$core$Tuple$mapSecond,
						pablohirafuji$elm_markdown$Markdown$Block$BlockQuote,
						A2(pablohirafuji$elm_markdown$Markdown$Block$parseReferences, refs, blocks)));
			case 'Custom':
				var customBlock = block.a;
				var blocks = block.b;
				return A2(
					elm$core$Tuple$mapSecond,
					function (a) {
						return A2(elm$core$List$cons, a, parsedAST);
					},
					A2(
						elm$core$Tuple$mapSecond,
						pablohirafuji$elm_markdown$Markdown$Block$Custom(customBlock),
						A2(pablohirafuji$elm_markdown$Markdown$Block$parseReferences, refs, blocks)));
			default:
				return _Utils_Tuple2(
					refs,
					A2(elm$core$List$cons, block, parsedAST));
		}
	});
var pablohirafuji$elm_markdown$Markdown$Block$parse = function (maybeOptions) {
	return A2(
		elm$core$Basics$composeR,
		elm$core$String$lines,
		A2(
			elm$core$Basics$composeR,
			function (a) {
				return A2(pablohirafuji$elm_markdown$Markdown$Block$incorporateLines, a, _List_Nil);
			},
			A2(
				elm$core$Basics$composeR,
				pablohirafuji$elm_markdown$Markdown$Block$parseReferences(elm$core$Dict$empty),
				A2(pablohirafuji$elm_markdown$Markdown$Block$parseInlines, maybeOptions, true))));
};
var pablohirafuji$elm_markdown$Markdown$Config$ParseUnsafe = {$: 'ParseUnsafe'};
var pablohirafuji$elm_markdown$Markdown$Inline$extractText = function (inlines) {
	return A3(elm$core$List$foldl, pablohirafuji$elm_markdown$Markdown$Inline$extractTextHelp, '', inlines);
};
var pablohirafuji$elm_markdown$Markdown$Inline$extractTextHelp = F2(
	function (inline, text) {
		switch (inline.$) {
			case 'Text':
				var str = inline.a;
				return _Utils_ap(text, str);
			case 'HardLineBreak':
				return text + ' ';
			case 'CodeInline':
				var str = inline.a;
				return _Utils_ap(text, str);
			case 'Link':
				var inlines = inline.c;
				return _Utils_ap(
					text,
					pablohirafuji$elm_markdown$Markdown$Inline$extractText(inlines));
			case 'Image':
				var inlines = inline.c;
				return _Utils_ap(
					text,
					pablohirafuji$elm_markdown$Markdown$Inline$extractText(inlines));
			case 'HtmlInline':
				var inlines = inline.c;
				return _Utils_ap(
					text,
					pablohirafuji$elm_markdown$Markdown$Inline$extractText(inlines));
			case 'Emphasis':
				var inlines = inline.b;
				return _Utils_ap(
					text,
					pablohirafuji$elm_markdown$Markdown$Inline$extractText(inlines));
			default:
				var inlines = inline.b;
				return _Utils_ap(
					text,
					pablohirafuji$elm_markdown$Markdown$Inline$extractText(inlines));
		}
	});
var author$project$Guide$Document$parse = F2(
	function (_n0, markdown) {
		var screenWidth = _n0.screenWidth;
		var widgets = _n0.widgets;
		var width = A2(elm$core$Basics$min, screenWidth - 24, 640);
		var screenType = (screenWidth > 600) ? author$project$Guide$Document$LargeScreen : author$project$Guide$Document$SmallScreen;
		var options = {rawHtml: pablohirafuji$elm_markdown$Markdown$Config$ParseUnsafe, softAsHardLineBreak: false};
		var blocks = A2(
			pablohirafuji$elm_markdown$Markdown$Block$parse,
			elm$core$Maybe$Just(options),
			markdown);
		if ((blocks.b && (blocks.a.$ === 'Heading')) && (blocks.a.b === 1)) {
			var _n2 = blocks.a;
			var inlines = _n2.c;
			var rest = blocks.b;
			return A3(
				elm$core$Result$map2,
				F2(
					function (titleText, bodyChunks) {
						return author$project$Guide$Document$Document(
							{
								chunks: A2(
									author$project$Guide$Document$compile,
									screenType,
									A2(
										elm$core$List$cons,
										author$project$Guide$Document$Title(titleText),
										bodyChunks)),
								screenType: screenType,
								title: pablohirafuji$elm_markdown$Markdown$Inline$extractText(inlines),
								width: width
							});
					}),
				A2(author$project$Guide$Document$parseText, inlines, _List_Nil),
				A3(
					author$project$Guide$Document$parseChunks,
					{
						screenType: screenType,
						widgets: elm$core$Dict$fromList(widgets)
					},
					rest,
					_List_Nil));
		} else {
			return elm$core$Result$Err('Markdown document must start with a level 1 header');
		}
	});
var author$project$Main$Error = function (a) {
	return {$: 'Error', a: a};
};
var author$project$Main$Loaded = function (a) {
	return {$: 'Loaded', a: a};
};
var author$project$Guide$Widget$Widget = function (a) {
	return {$: 'Widget', a: a};
};
var elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var mdgriffith$elm_ui$Internal$Model$Empty = {$: 'Empty'};
var mdgriffith$elm_ui$Internal$Model$map = F2(
	function (fn, el) {
		switch (el.$) {
			case 'Styled':
				var styled = el.a;
				return mdgriffith$elm_ui$Internal$Model$Styled(
					{
						html: F2(
							function (add, context) {
								return A2(
									elm$virtual_dom$VirtualDom$map,
									fn,
									A2(styled.html, add, context));
							}),
						styles: styled.styles
					});
			case 'Unstyled':
				var html = el.a;
				return mdgriffith$elm_ui$Internal$Model$Unstyled(
					A2(
						elm$core$Basics$composeL,
						elm$virtual_dom$VirtualDom$map(fn),
						html));
			case 'Text':
				var str = el.a;
				return mdgriffith$elm_ui$Internal$Model$Text(str);
			default:
				return mdgriffith$elm_ui$Internal$Model$Empty;
		}
	});
var mdgriffith$elm_ui$Element$map = mdgriffith$elm_ui$Internal$Model$map;
var author$project$Guide$Widget$interactive = function (config) {
	return author$project$Guide$Widget$Widget(
		A2(
			mdgriffith$elm_ui$Element$map,
			function (message) {
				return author$project$Guide$Widget$interactive(
					{
						init: A2(config.update, message, config.init),
						update: config.update,
						view: config.view
					});
			},
			config.view(config.init)));
};
var author$project$Main$Decrement = {$: 'Decrement'};
var author$project$Main$Increment = {$: 'Increment'};
var mdgriffith$elm_ui$Internal$Model$AlignX = function (a) {
	return {$: 'AlignX', a: a};
};
var mdgriffith$elm_ui$Internal$Model$CenterX = {$: 'CenterX'};
var mdgriffith$elm_ui$Element$centerX = mdgriffith$elm_ui$Internal$Model$AlignX(mdgriffith$elm_ui$Internal$Model$CenterX);
var elm$json$Json$Encode$bool = _Json_wrap;
var elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			elm$json$Json$Encode$bool(bool));
	});
var elm$html$Html$Attributes$disabled = elm$html$Html$Attributes$boolProperty('disabled');
var elm$html$Html$Attributes$tabindex = function (n) {
	return A2(
		_VirtualDom_attribute,
		'tabIndex',
		elm$core$String$fromInt(n));
};
var mdgriffith$elm_ui$Internal$Flag$cursor = mdgriffith$elm_ui$Internal$Flag$flag(21);
var mdgriffith$elm_ui$Element$pointer = A2(mdgriffith$elm_ui$Internal$Model$Class, mdgriffith$elm_ui$Internal$Flag$cursor, mdgriffith$elm_ui$Internal$Style$classes.cursorPointer);
var elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var elm$html$Html$Events$onClick = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'click',
		elm$json$Json$Decode$succeed(msg));
};
var mdgriffith$elm_ui$Element$Events$onClick = A2(elm$core$Basics$composeL, mdgriffith$elm_ui$Internal$Model$Attr, elm$html$Html$Events$onClick);
var mdgriffith$elm_ui$Element$Input$hasFocusStyle = function (attr) {
	if (((attr.$ === 'StyleClass') && (attr.b.$ === 'PseudoSelector')) && (attr.b.a.$ === 'Focus')) {
		var _n1 = attr.b;
		var _n2 = _n1.a;
		return true;
	} else {
		return false;
	}
};
var mdgriffith$elm_ui$Internal$Model$NoAttribute = {$: 'NoAttribute'};
var mdgriffith$elm_ui$Element$Input$focusDefault = function (attrs) {
	return A2(elm$core$List$any, mdgriffith$elm_ui$Element$Input$hasFocusStyle, attrs) ? mdgriffith$elm_ui$Internal$Model$NoAttribute : mdgriffith$elm_ui$Internal$Model$htmlClass('focusable');
};
var mdgriffith$elm_ui$Element$Input$enter = 'Enter';
var elm$virtual_dom$VirtualDom$MayPreventDefault = function (a) {
	return {$: 'MayPreventDefault', a: a};
};
var elm$html$Html$Events$preventDefaultOn = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$MayPreventDefault(decoder));
	});
var elm$json$Json$Decode$andThen = _Json_andThen;
var elm$json$Json$Decode$fail = _Json_fail;
var elm$json$Json$Decode$field = _Json_decodeField;
var elm$json$Json$Decode$string = _Json_decodeString;
var mdgriffith$elm_ui$Element$Input$onKey = F2(
	function (desiredCode, msg) {
		var decode = function (code) {
			return _Utils_eq(code, desiredCode) ? elm$json$Json$Decode$succeed(msg) : elm$json$Json$Decode$fail('Not the enter key');
		};
		var isKey = A2(
			elm$json$Json$Decode$andThen,
			decode,
			A2(elm$json$Json$Decode$field, 'key', elm$json$Json$Decode$string));
		return mdgriffith$elm_ui$Internal$Model$Attr(
			A2(
				elm$html$Html$Events$preventDefaultOn,
				'keyup',
				A2(
					elm$json$Json$Decode$map,
					function (fired) {
						return _Utils_Tuple2(fired, true);
					},
					isKey)));
	});
var mdgriffith$elm_ui$Element$Input$onEnter = function (msg) {
	return A2(mdgriffith$elm_ui$Element$Input$onKey, mdgriffith$elm_ui$Element$Input$enter, msg);
};
var mdgriffith$elm_ui$Internal$Model$Button = {$: 'Button'};
var mdgriffith$elm_ui$Element$Input$button = F2(
	function (attrs, _n0) {
		var onPress = _n0.onPress;
		var label = _n0.label;
		return A4(
			mdgriffith$elm_ui$Internal$Model$element,
			mdgriffith$elm_ui$Internal$Model$asEl,
			mdgriffith$elm_ui$Internal$Model$div,
			A2(
				elm$core$List$cons,
				mdgriffith$elm_ui$Element$width(mdgriffith$elm_ui$Element$shrink),
				A2(
					elm$core$List$cons,
					mdgriffith$elm_ui$Element$height(mdgriffith$elm_ui$Element$shrink),
					A2(
						elm$core$List$cons,
						mdgriffith$elm_ui$Internal$Model$htmlClass(mdgriffith$elm_ui$Internal$Style$classes.contentCenterX + (' ' + (mdgriffith$elm_ui$Internal$Style$classes.contentCenterY + (' ' + (mdgriffith$elm_ui$Internal$Style$classes.seButton + (' ' + mdgriffith$elm_ui$Internal$Style$classes.noTextSelection)))))),
						A2(
							elm$core$List$cons,
							mdgriffith$elm_ui$Element$pointer,
							A2(
								elm$core$List$cons,
								mdgriffith$elm_ui$Element$Input$focusDefault(attrs),
								A2(
									elm$core$List$cons,
									mdgriffith$elm_ui$Internal$Model$Describe(mdgriffith$elm_ui$Internal$Model$Button),
									A2(
										elm$core$List$cons,
										mdgriffith$elm_ui$Internal$Model$Attr(
											elm$html$Html$Attributes$tabindex(0)),
										function () {
											if (onPress.$ === 'Nothing') {
												return A2(
													elm$core$List$cons,
													mdgriffith$elm_ui$Internal$Model$Attr(
														elm$html$Html$Attributes$disabled(true)),
													attrs);
											} else {
												var msg = onPress.a;
												return A2(
													elm$core$List$cons,
													mdgriffith$elm_ui$Element$Events$onClick(msg),
													A2(
														elm$core$List$cons,
														mdgriffith$elm_ui$Element$Input$onEnter(msg),
														attrs));
											}
										}()))))))),
			mdgriffith$elm_ui$Internal$Model$Unkeyed(
				_List_fromArray(
					[label])));
	});
var author$project$Main$counter = author$project$Guide$Widget$interactive(
	{
		init: 5,
		update: F2(
			function (message, model) {
				if (message.$ === 'Increment') {
					return model + 1;
				} else {
					return model - 1;
				}
			}),
		view: function (count) {
			return A2(
				mdgriffith$elm_ui$Element$row,
				_List_fromArray(
					[
						mdgriffith$elm_ui$Element$width(mdgriffith$elm_ui$Element$fill)
					]),
				_List_fromArray(
					[
						A2(
						mdgriffith$elm_ui$Element$row,
						_List_fromArray(
							[mdgriffith$elm_ui$Element$centerX]),
						_List_fromArray(
							[
								A2(
								mdgriffith$elm_ui$Element$Input$button,
								_List_Nil,
								{
									label: mdgriffith$elm_ui$Element$text('+'),
									onPress: elm$core$Maybe$Just(author$project$Main$Increment)
								}),
								mdgriffith$elm_ui$Element$text(
								elm$core$String$fromInt(count)),
								A2(
								mdgriffith$elm_ui$Element$Input$button,
								_List_Nil,
								{
									label: mdgriffith$elm_ui$Element$text('-'),
									onPress: elm$core$Maybe$Just(author$project$Main$Decrement)
								})
							]))
					]));
		}
	});
var author$project$Test$md = '# Title with `inline code`\r\n\r\nSome `inline code` followed by some _italic_ text.\r\n\r\n## A section with `some inline-code`\r\n\r\nContaining some `more inline code`.\r\n\r\nAlso some bullets:\r\n\r\n* First bullet\r\n* Second bullet\r\n  * With an indented bullet that has a custom widget\r\n    \r\n    <counter/>\r\n\r\n  * And another indented bullet with a code block:\r\n    \r\n        some code\r\n        some more code\r\n            some indented code\r\n\r\n  * A bullet after the code block!\r\n\r\n* And finally a third bullet\r\n* Plus a fourth bullet just for fun with some _italic text_ and `long inline code` probably\r\n  splitting over multiple lines because I made this bullet **really** long\r\n\r\n## Another _section_\r\n    \r\nWith some **bold** text, and a custom element:\r\n\r\n<counter/>\r\n\r\nA code block:\r\n\r\n    code\r\n    more code\r\n        indented code\r\n    unindented code\r\n\r\n### A subsection with `some inline-code`\r\n\r\nWith another custom element:\r\n\r\n### `A weird subsection`\r\n\r\nJust inline code in the title!\r\n\r\n<counter/>\r\n';
var elm$core$Platform$Cmd$batch = _Platform_batch;
var elm$core$Platform$Cmd$none = elm$core$Platform$Cmd$batch(_List_Nil);
var author$project$Main$init = function (flags) {
	var widgets = _List_fromArray(
		[
			_Utils_Tuple2('counter', author$project$Main$counter)
		]);
	var model = function () {
		var _n0 = A2(
			author$project$Guide$Document$parse,
			{screenWidth: flags.width, widgets: widgets},
			author$project$Test$md);
		if (_n0.$ === 'Ok') {
			var document = _n0.a;
			return author$project$Main$Loaded(document);
		} else {
			var message = _n0.a;
			return author$project$Main$Error(message);
		}
	}();
	return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
};
var author$project$Main$update = F2(
	function (newDocument, model) {
		return _Utils_Tuple2(
			author$project$Main$Loaded(newDocument),
			elm$core$Platform$Cmd$none);
	});
var author$project$Guide$Document$title = function (_n0) {
	var document = _n0.a;
	return document.title;
};
var mdgriffith$elm_ui$Internal$Model$Serif = {$: 'Serif'};
var mdgriffith$elm_ui$Element$Font$serif = mdgriffith$elm_ui$Internal$Model$Serif;
var author$project$Guide$Document$merriweather = mdgriffith$elm_ui$Element$Font$family(
	_List_fromArray(
		[
			mdgriffith$elm_ui$Element$Font$typeface('Merriweather'),
			mdgriffith$elm_ui$Element$Font$serif
		]));
var author$project$Guide$Document$updateWidget = F3(
	function (givenId, newWidget, chunks) {
		return A2(
			elm$core$List$map,
			function (chunk) {
				switch (chunk.$) {
					case 'Static':
						return chunk;
					case 'Interactive':
						var widgetId = chunk.a;
						var currentWidget = chunk.b;
						return _Utils_eq(widgetId, givenId) ? A2(author$project$Guide$Document$Interactive, widgetId, newWidget) : chunk;
					default:
						var bullets = chunk.a;
						return author$project$Guide$Document$CompiledBullets(
							A2(
								elm$core$List$map,
								A2(author$project$Guide$Document$updateWidget, givenId, newWidget),
								bullets));
				}
			},
			chunks);
	});
var elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var elm$svg$Svg$circle = elm$svg$Svg$trustedNode('circle');
var elm$svg$Svg$svg = elm$svg$Svg$trustedNode('svg');
var elm$svg$Svg$Attributes$cx = _VirtualDom_attribute('cx');
var elm$svg$Svg$Attributes$cy = _VirtualDom_attribute('cy');
var elm$svg$Svg$Attributes$fill = _VirtualDom_attribute('fill');
var elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var elm$svg$Svg$Attributes$r = _VirtualDom_attribute('r');
var elm$svg$Svg$Attributes$stroke = _VirtualDom_attribute('stroke');
var elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var elm$core$Basics$always = F2(
	function (a, _n0) {
		return a;
	});
var mdgriffith$elm_ui$Internal$Model$unstyled = A2(elm$core$Basics$composeL, mdgriffith$elm_ui$Internal$Model$Unstyled, elm$core$Basics$always);
var mdgriffith$elm_ui$Element$html = mdgriffith$elm_ui$Internal$Model$unstyled;
var author$project$Guide$Document$bulletIcon = function (_n0) {
	var screenType = _n0.screenType;
	var topLevel = _n0.topLevel;
	var rightPadding = function () {
		if (screenType.$ === 'LargeScreen') {
			return 8;
		} else {
			return 6;
		}
	}();
	var radius = function () {
		if (screenType.$ === 'LargeScreen') {
			return 3.25;
		} else {
			return 2.75;
		}
	}();
	var leftPadding = function () {
		if (topLevel) {
			if (screenType.$ === 'LargeScreen') {
				return 20;
			} else {
				return 12;
			}
		} else {
			return rightPadding;
		}
	}();
	var height = function () {
		if (screenType.$ === 'LargeScreen') {
			return 6.5;
		} else {
			return 6.5;
		}
	}();
	var halfWidth = elm$core$Basics$ceiling(radius);
	return A2(
		mdgriffith$elm_ui$Element$el,
		_List_fromArray(
			[
				mdgriffith$elm_ui$Element$paddingEach(
				{bottom: 0, left: leftPadding, right: rightPadding, top: 0}),
				mdgriffith$elm_ui$Element$Font$bold
			]),
		mdgriffith$elm_ui$Element$html(
			A2(
				elm$svg$Svg$svg,
				_List_fromArray(
					[
						elm$svg$Svg$Attributes$width(
						elm$core$String$fromInt(2 * halfWidth)),
						elm$svg$Svg$Attributes$height(
						elm$core$String$fromInt(
							author$project$Guide$Document$fontSizes(screenType).body))
					]),
				_List_fromArray(
					[
						A2(
						elm$svg$Svg$circle,
						_List_fromArray(
							[
								elm$svg$Svg$Attributes$cx(
								elm$core$String$fromInt(halfWidth)),
								elm$svg$Svg$Attributes$cy(
								elm$core$String$fromFloat(
									author$project$Guide$Document$fontSizes(screenType).body - height)),
								elm$svg$Svg$Attributes$r(
								elm$core$String$fromFloat(radius)),
								elm$svg$Svg$Attributes$fill('black'),
								elm$svg$Svg$Attributes$stroke('none')
							]),
						_List_Nil)
					]))));
};
var author$project$Guide$Document$bulletSpacing = 8;
var author$project$Guide$Document$topLevelSpacing = 12;
var author$project$Guide$Document$widthFill = mdgriffith$elm_ui$Element$width(mdgriffith$elm_ui$Element$fill);
var author$project$Guide$Widget$view = F2(
	function (toMessage, _n0) {
		var element = _n0.a;
		return A2(mdgriffith$elm_ui$Element$map, toMessage, element);
	});
var elm$core$Basics$never = function (_n0) {
	never:
	while (true) {
		var nvr = _n0.a;
		var $temp$_n0 = nvr;
		_n0 = $temp$_n0;
		continue never;
	}
};
var mdgriffith$elm_ui$Internal$Model$AlignY = function (a) {
	return {$: 'AlignY', a: a};
};
var mdgriffith$elm_ui$Internal$Model$Top = {$: 'Top'};
var mdgriffith$elm_ui$Element$alignTop = mdgriffith$elm_ui$Internal$Model$AlignY(mdgriffith$elm_ui$Internal$Model$Top);
var mdgriffith$elm_ui$Internal$Model$Max = F2(
	function (a, b) {
		return {$: 'Max', a: a, b: b};
	});
var mdgriffith$elm_ui$Element$maximum = F2(
	function (i, l) {
		return A2(mdgriffith$elm_ui$Internal$Model$Max, i, l);
	});
var mdgriffith$elm_ui$Internal$Model$Min = F2(
	function (a, b) {
		return {$: 'Min', a: a, b: b};
	});
var mdgriffith$elm_ui$Element$minimum = F2(
	function (i, l) {
		return A2(mdgriffith$elm_ui$Internal$Model$Min, i, l);
	});
var mdgriffith$elm_ui$Internal$Model$AsTextColumn = {$: 'AsTextColumn'};
var mdgriffith$elm_ui$Internal$Model$asTextColumn = mdgriffith$elm_ui$Internal$Model$AsTextColumn;
var mdgriffith$elm_ui$Element$textColumn = F2(
	function (attrs, children) {
		return A4(
			mdgriffith$elm_ui$Internal$Model$element,
			mdgriffith$elm_ui$Internal$Model$asTextColumn,
			mdgriffith$elm_ui$Internal$Model$div,
			A2(
				elm$core$List$cons,
				mdgriffith$elm_ui$Element$width(
					A2(
						mdgriffith$elm_ui$Element$maximum,
						750,
						A2(mdgriffith$elm_ui$Element$minimum, 500, mdgriffith$elm_ui$Element$fill))),
				attrs),
			mdgriffith$elm_ui$Internal$Model$Unkeyed(children));
	});
var author$project$Guide$Document$bulletedItem = F2(
	function (config, chunks) {
		return A2(
			mdgriffith$elm_ui$Element$row,
			_List_fromArray(
				[author$project$Guide$Document$widthFill]),
			_List_fromArray(
				[
					A2(
					mdgriffith$elm_ui$Element$el,
					_List_fromArray(
						[mdgriffith$elm_ui$Element$alignTop]),
					author$project$Guide$Document$bulletIcon(config)),
					A2(
					author$project$Guide$Document$viewChunks,
					_Utils_update(
						config,
						{topLevel: false}),
					chunks)
				]));
	});
var author$project$Guide$Document$viewBullets = F2(
	function (config, bullets) {
		return A2(
			mdgriffith$elm_ui$Element$column,
			_List_fromArray(
				[
					mdgriffith$elm_ui$Element$spacing(author$project$Guide$Document$bulletSpacing),
					author$project$Guide$Document$widthFill
				]),
			A2(
				elm$core$List$map,
				author$project$Guide$Document$bulletedItem(config),
				bullets));
	});
var author$project$Guide$Document$viewChunk = F2(
	function (config, chunk) {
		switch (chunk.$) {
			case 'Static':
				var textContext = chunk.a;
				var element = chunk.b;
				return A2(mdgriffith$elm_ui$Element$map, elm$core$Basics$never, element);
			case 'Interactive':
				var id = chunk.a;
				var widget = chunk.b;
				return A2(
					author$project$Guide$Widget$view,
					function (newWidget) {
						return _Utils_Tuple2(id, newWidget);
					},
					widget);
			default:
				var bullets = chunk.a;
				return A2(author$project$Guide$Document$viewBullets, config, bullets);
		}
	});
var author$project$Guide$Document$viewChunks = F2(
	function (config, chunks) {
		var spacing = config.topLevel ? author$project$Guide$Document$topLevelSpacing : author$project$Guide$Document$bulletSpacing;
		return A2(
			mdgriffith$elm_ui$Element$textColumn,
			_List_fromArray(
				[
					mdgriffith$elm_ui$Element$spacing(spacing),
					author$project$Guide$Document$widthFill
				]),
			A2(
				elm$core$List$map,
				author$project$Guide$Document$viewChunk(config),
				chunks));
	});
var mdgriffith$elm_ui$Internal$Model$Px = function (a) {
	return {$: 'Px', a: a};
};
var mdgriffith$elm_ui$Element$px = mdgriffith$elm_ui$Internal$Model$Px;
var author$project$Guide$Document$view = F2(
	function (attributes, _n0) {
		var document = _n0.a;
		var widthAttribute = mdgriffith$elm_ui$Element$width(
			mdgriffith$elm_ui$Element$px(document.width));
		var fontSizeAttribute = mdgriffith$elm_ui$Element$Font$size(
			author$project$Guide$Document$fontSizes(document.screenType).body);
		return A2(
			mdgriffith$elm_ui$Element$el,
			A2(
				elm$core$List$cons,
				author$project$Guide$Document$merriweather,
				A2(
					elm$core$List$cons,
					fontSizeAttribute,
					A2(elm$core$List$cons, widthAttribute, attributes))),
			A2(
				mdgriffith$elm_ui$Element$map,
				function (_n1) {
					var id = _n1.a;
					var widget = _n1.b;
					return author$project$Guide$Document$Document(
						{
							chunks: A3(author$project$Guide$Document$updateWidget, id, widget, document.chunks),
							screenType: document.screenType,
							title: document.title,
							width: document.width
						});
				},
				A2(
					author$project$Guide$Document$viewChunks,
					{screenType: document.screenType, topLevel: true},
					document.chunks)));
	});
var mdgriffith$elm_ui$Internal$Model$OnlyDynamic = F2(
	function (a, b) {
		return {$: 'OnlyDynamic', a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Model$StaticRootAndDynamic = F2(
	function (a, b) {
		return {$: 'StaticRootAndDynamic', a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Model$AllowHover = {$: 'AllowHover'};
var mdgriffith$elm_ui$Internal$Model$Layout = {$: 'Layout'};
var mdgriffith$elm_ui$Internal$Model$focusDefaultStyle = {
	backgroundColor: elm$core$Maybe$Nothing,
	borderColor: elm$core$Maybe$Nothing,
	shadow: elm$core$Maybe$Just(
		{
			blur: 3,
			color: A4(mdgriffith$elm_ui$Internal$Model$Rgba, 155 / 255, 203 / 255, 1, 1),
			offset: _Utils_Tuple2(0, 0),
			size: 3
		})
};
var mdgriffith$elm_ui$Internal$Model$optionsToRecord = function (options) {
	var combine = F2(
		function (opt, record) {
			switch (opt.$) {
				case 'HoverOption':
					var hoverable = opt.a;
					var _n4 = record.hover;
					if (_n4.$ === 'Nothing') {
						return _Utils_update(
							record,
							{
								hover: elm$core$Maybe$Just(hoverable)
							});
					} else {
						return record;
					}
				case 'FocusStyleOption':
					var focusStyle = opt.a;
					var _n5 = record.focus;
					if (_n5.$ === 'Nothing') {
						return _Utils_update(
							record,
							{
								focus: elm$core$Maybe$Just(focusStyle)
							});
					} else {
						return record;
					}
				default:
					var renderMode = opt.a;
					var _n6 = record.mode;
					if (_n6.$ === 'Nothing') {
						return _Utils_update(
							record,
							{
								mode: elm$core$Maybe$Just(renderMode)
							});
					} else {
						return record;
					}
			}
		});
	var andFinally = function (record) {
		return {
			focus: function () {
				var _n0 = record.focus;
				if (_n0.$ === 'Nothing') {
					return mdgriffith$elm_ui$Internal$Model$focusDefaultStyle;
				} else {
					var focusable = _n0.a;
					return focusable;
				}
			}(),
			hover: function () {
				var _n1 = record.hover;
				if (_n1.$ === 'Nothing') {
					return mdgriffith$elm_ui$Internal$Model$AllowHover;
				} else {
					var hoverable = _n1.a;
					return hoverable;
				}
			}(),
			mode: function () {
				var _n2 = record.mode;
				if (_n2.$ === 'Nothing') {
					return mdgriffith$elm_ui$Internal$Model$Layout;
				} else {
					var actualMode = _n2.a;
					return actualMode;
				}
			}()
		};
	};
	return andFinally(
		A3(
			elm$core$List$foldr,
			combine,
			{focus: elm$core$Maybe$Nothing, hover: elm$core$Maybe$Nothing, mode: elm$core$Maybe$Nothing},
			options));
};
var mdgriffith$elm_ui$Internal$Model$toHtml = F2(
	function (mode, el) {
		switch (el.$) {
			case 'Unstyled':
				var html = el.a;
				return html(mdgriffith$elm_ui$Internal$Model$asEl);
			case 'Styled':
				var styles = el.a.styles;
				var html = el.a.html;
				return A2(
					html,
					mode(styles),
					mdgriffith$elm_ui$Internal$Model$asEl);
			case 'Text':
				var text = el.a;
				return mdgriffith$elm_ui$Internal$Model$textElement(text);
			default:
				return mdgriffith$elm_ui$Internal$Model$textElement('');
		}
	});
var mdgriffith$elm_ui$Internal$Model$renderRoot = F3(
	function (optionList, attributes, child) {
		var options = mdgriffith$elm_ui$Internal$Model$optionsToRecord(optionList);
		var embedStyle = function () {
			var _n0 = options.mode;
			if (_n0.$ === 'NoStaticStyleSheet') {
				return mdgriffith$elm_ui$Internal$Model$OnlyDynamic(options);
			} else {
				return mdgriffith$elm_ui$Internal$Model$StaticRootAndDynamic(options);
			}
		}();
		return A2(
			mdgriffith$elm_ui$Internal$Model$toHtml,
			embedStyle,
			A4(
				mdgriffith$elm_ui$Internal$Model$element,
				mdgriffith$elm_ui$Internal$Model$asEl,
				mdgriffith$elm_ui$Internal$Model$div,
				attributes,
				mdgriffith$elm_ui$Internal$Model$Unkeyed(
					_List_fromArray(
						[child]))));
	});
var mdgriffith$elm_ui$Internal$Flag$fontColor = mdgriffith$elm_ui$Internal$Flag$flag(14);
var mdgriffith$elm_ui$Internal$Model$rootStyle = function () {
	var families = _List_fromArray(
		[
			mdgriffith$elm_ui$Internal$Model$Typeface('Open Sans'),
			mdgriffith$elm_ui$Internal$Model$Typeface('Helvetica'),
			mdgriffith$elm_ui$Internal$Model$Typeface('Verdana'),
			mdgriffith$elm_ui$Internal$Model$SansSerif
		]);
	return _List_fromArray(
		[
			A2(
			mdgriffith$elm_ui$Internal$Model$StyleClass,
			mdgriffith$elm_ui$Internal$Flag$bgColor,
			A3(
				mdgriffith$elm_ui$Internal$Model$Colored,
				'bg-color-' + mdgriffith$elm_ui$Internal$Model$formatColorClass(
					A4(mdgriffith$elm_ui$Internal$Model$Rgba, 1, 1, 1, 0)),
				'background-color',
				A4(mdgriffith$elm_ui$Internal$Model$Rgba, 1, 1, 1, 0))),
			A2(
			mdgriffith$elm_ui$Internal$Model$StyleClass,
			mdgriffith$elm_ui$Internal$Flag$fontColor,
			A3(
				mdgriffith$elm_ui$Internal$Model$Colored,
				'font-color-' + mdgriffith$elm_ui$Internal$Model$formatColorClass(
					A4(mdgriffith$elm_ui$Internal$Model$Rgba, 0, 0, 0, 1)),
				'color',
				A4(mdgriffith$elm_ui$Internal$Model$Rgba, 0, 0, 0, 1))),
			A2(
			mdgriffith$elm_ui$Internal$Model$StyleClass,
			mdgriffith$elm_ui$Internal$Flag$fontSize,
			mdgriffith$elm_ui$Internal$Model$FontSize(20)),
			A2(
			mdgriffith$elm_ui$Internal$Model$StyleClass,
			mdgriffith$elm_ui$Internal$Flag$fontFamily,
			A2(
				mdgriffith$elm_ui$Internal$Model$FontFamily,
				A3(elm$core$List$foldl, mdgriffith$elm_ui$Internal$Model$renderFontClassName, 'font-', families),
				families))
		]);
}();
var mdgriffith$elm_ui$Element$layoutWith = F3(
	function (_n0, attrs, child) {
		var options = _n0.options;
		return A3(
			mdgriffith$elm_ui$Internal$Model$renderRoot,
			options,
			A2(
				elm$core$List$cons,
				mdgriffith$elm_ui$Internal$Model$htmlClass(
					A2(
						elm$core$String$join,
						' ',
						_List_fromArray(
							[mdgriffith$elm_ui$Internal$Style$classes.root, mdgriffith$elm_ui$Internal$Style$classes.any, mdgriffith$elm_ui$Internal$Style$classes.single]))),
				_Utils_ap(mdgriffith$elm_ui$Internal$Model$rootStyle, attrs)),
			child);
	});
var mdgriffith$elm_ui$Element$layout = mdgriffith$elm_ui$Element$layoutWith(
	{options: _List_Nil});
var author$project$Main$view = function (model) {
	if (model.$ === 'Loaded') {
		var document = model.a;
		return {
			body: _List_fromArray(
				[
					A2(
					mdgriffith$elm_ui$Element$layout,
					_List_fromArray(
						[
							mdgriffith$elm_ui$Element$width(mdgriffith$elm_ui$Element$fill)
						]),
					A2(
						author$project$Guide$Document$view,
						_List_fromArray(
							[mdgriffith$elm_ui$Element$centerX]),
						document))
				]),
			title: author$project$Guide$Document$title(document)
		};
	} else {
		var message = model.a;
		return {
			body: _List_fromArray(
				[
					elm$html$Html$text(message)
				]),
			title: 'Error!'
		};
	}
};
var elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var elm$core$Task$succeed = _Scheduler_succeed;
var elm$core$Task$init = elm$core$Task$succeed(_Utils_Tuple0);
var elm$core$Task$andThen = _Scheduler_andThen;
var elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			elm$core$Task$andThen,
			function (a) {
				return elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			elm$core$Task$andThen,
			function (a) {
				return A2(
					elm$core$Task$andThen,
					function (b) {
						return elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var elm$core$Task$sequence = function (tasks) {
	return A3(
		elm$core$List$foldr,
		elm$core$Task$map2(elm$core$List$cons),
		elm$core$Task$succeed(_List_Nil),
		tasks);
};
var elm$core$Platform$sendToApp = _Platform_sendToApp;
var elm$core$Task$spawnCmd = F2(
	function (router, _n0) {
		var task = _n0.a;
		return _Scheduler_spawn(
			A2(
				elm$core$Task$andThen,
				elm$core$Platform$sendToApp(router),
				task));
	});
var elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			elm$core$Task$map,
			function (_n0) {
				return _Utils_Tuple0;
			},
			elm$core$Task$sequence(
				A2(
					elm$core$List$map,
					elm$core$Task$spawnCmd(router),
					commands)));
	});
var elm$core$Task$onSelfMsg = F3(
	function (_n0, _n1, _n2) {
		return elm$core$Task$succeed(_Utils_Tuple0);
	});
var elm$core$Task$cmdMap = F2(
	function (tagger, _n0) {
		var task = _n0.a;
		return elm$core$Task$Perform(
			A2(elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager(elm$core$Task$init, elm$core$Task$onEffects, elm$core$Task$onSelfMsg, elm$core$Task$cmdMap);
var elm$core$Task$command = _Platform_leaf('Task');
var elm$core$Task$perform = F2(
	function (toMessage, task) {
		return elm$core$Task$command(
			elm$core$Task$Perform(
				A2(elm$core$Task$map, toMessage, task)));
	});
var elm$url$Url$Http = {$: 'Http'};
var elm$url$Url$Https = {$: 'Https'};
var elm$core$String$indexes = _String_indexes;
var elm$core$String$contains = _String_contains;
var elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if (elm$core$String$isEmpty(str) || A2(elm$core$String$contains, '@', str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, ':', str);
			if (!_n0.b) {
				return elm$core$Maybe$Just(
					A6(elm$url$Url$Url, protocol, str, elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_n0.b.b) {
					var i = _n0.a;
					var _n1 = elm$core$String$toInt(
						A2(elm$core$String$dropLeft, i + 1, str));
					if (_n1.$ === 'Nothing') {
						return elm$core$Maybe$Nothing;
					} else {
						var port_ = _n1;
						return elm$core$Maybe$Just(
							A6(
								elm$url$Url$Url,
								protocol,
								A2(elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return elm$core$Maybe$Nothing;
				}
			}
		}
	});
var elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '/', str);
			if (!_n0.b) {
				return A5(elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _n0.a;
				return A5(
					elm$url$Url$chompBeforePath,
					protocol,
					A2(elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '?', str);
			if (!_n0.b) {
				return A4(elm$url$Url$chompBeforeQuery, protocol, elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _n0.a;
				return A4(
					elm$url$Url$chompBeforeQuery,
					protocol,
					elm$core$Maybe$Just(
						A2(elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '#', str);
			if (!_n0.b) {
				return A3(elm$url$Url$chompBeforeFragment, protocol, elm$core$Maybe$Nothing, str);
			} else {
				var i = _n0.a;
				return A3(
					elm$url$Url$chompBeforeFragment,
					protocol,
					elm$core$Maybe$Just(
						A2(elm$core$String$dropLeft, i + 1, str)),
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$fromString = function (str) {
	return A2(elm$core$String$startsWith, 'http://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		elm$url$Url$Http,
		A2(elm$core$String$dropLeft, 7, str)) : (A2(elm$core$String$startsWith, 'https://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		elm$url$Url$Https,
		A2(elm$core$String$dropLeft, 8, str)) : elm$core$Maybe$Nothing);
};
var elm$browser$Browser$document = _Browser_document;
var elm$core$Platform$Sub$batch = _Platform_batch;
var elm$core$Platform$Sub$none = elm$core$Platform$Sub$batch(_List_Nil);
var elm$json$Json$Decode$int = _Json_decodeInt;
var author$project$Main$main = elm$browser$Browser$document(
	{
		init: author$project$Main$init,
		subscriptions: elm$core$Basics$always(elm$core$Platform$Sub$none),
		update: author$project$Main$update,
		view: author$project$Main$view
	});
_Platform_export({'Main':{'init':author$project$Main$main(
	A2(
		elm$json$Json$Decode$andThen,
		function (width) {
			return A2(
				elm$json$Json$Decode$andThen,
				function (height) {
					return elm$json$Json$Decode$succeed(
						{height: height, width: width});
				},
				A2(elm$json$Json$Decode$field, 'height', elm$json$Json$Decode$int));
		},
		A2(elm$json$Json$Decode$field, 'width', elm$json$Json$Decode$int)))(0)}});}(this));