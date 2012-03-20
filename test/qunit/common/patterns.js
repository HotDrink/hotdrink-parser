(function () {

  var TentativeTest = function () {
    this.failed = 0;
  };

  TentativeTest.prototype = {};

  TentativeTest.prototype.ok = function (result, desc) {
    if (!result) ++this.failed;
  };

  TentativeTest.prototype.strictEqual = function (a, b, desc) {
    this.ok(a === b, desc);
  };

  TentativeTest.prototype.passed = function () {
    return this.failed === 0;
  };

  /**
   */
  var Pattern = function () {
  };

  Pattern.prototype = {};

  Pattern.prototype.compare = function () {
    test.ok(false,
      "error: incomplete pattern");
  };

  var Key = function (key) {
    this.key = key;
  };

  Key.prototype = new Pattern();

  Key.prototype.compare = function (cand, test, keyMap) {
    if (!keyMap[this.key]) {
      test.ok(false,
        "error: key '" + this.key + "' has not been mapped");
    } else {
      test.strictEqual(cand, keyMap[this.key],
        "candidate matches key '" + this.key + "'");
    }
  };

  /**
   */
  var Min = function () {};

  Min.prototype = new Pattern();

  Min.prototype.initialize = function (pattern, keyOrder) {
    this.pattern = pattern;

    /* TODO: Check that the keys in keyOrder exist in pattern? */
    if (!keyOrder) {
      /* TODO: Remove this check after we remove Prototype. */
      if (Array.isArray(pattern)) {
        keyOrder = [];
        for (var i = 0; i < pattern.length; ++i) {
          keyOrder.push(i);
        }
      } else {
        keyOrder = Object.keys(pattern);
      }
    }
    this.keyOrder = keyOrder;
  };

  var matchProperty = function (key, proof, cand, test, keyMap) {
    if (proof instanceof Pattern) {
      proof.compare(cand, test, keyMap);
    } else {
      test.strictEqual(cand, proof,
        "property '" + key + "' matches expected value");
    }
  };

  Min.prototype.compare = function (cand, test, keyMap) {
    keyMap = keyMap || {};

    for (var i = 0; i < this.keyOrder.length; ++i) {
      var key = this.keyOrder[i];

      if (!(key in cand)) {
        test.ok(false,
          "error: candidate missing property '" + key + "'");
        continue;
      }

      matchProperty(key, this.pattern[key], cand[key], test,  keyMap);
    }
  };

  /**
   */
  var Exact = function () {};

  Exact.prototype = new Min();

  Exact.prototype.compare = function (cand, test, keyMap) {
    keyMap = keyMap || {};

    Min.prototype.compare.call(this, cand, test, keyMap);

    for (var key in cand) {
      if (!cand.hasOwnProperty(key)) continue;
      if (!(key in this.pattern)) {
        test.ok(false,
          "error: candidate has extra property ('" + key + "')");
      }
    }
  }

  /**
   */
  var MinSet = function () {};

  MinSet.prototype = new Pattern();

  MinSet.prototype.compare = function (cand, test, keyMap) {
    keyMap = keyMap || {};

    var clone = Object.clone(cand);

    for (var i = 0; i < this.keyOrder.length; ++i) {
      var pkey = this.keyOrder[i];

      for (var ckey in clone) {
        if (!clone.hasOwnProperty(ckey)) continue;

        var ttest = new TentativeTest();
        var tkeyMap = Object.clone(keyMap);
        tkeyMap[pkey] = ckey;

        matchProperty(ckey, this.pattern[pkey], clone[ckey], ttest, tkeyMap);

        if (ttest.passed()) {
          Object.extend(keyMap, tkeyMap);
          delete clone[ckey];
          break;
        }
      }

      if (!(pkey in keyMap)) {
        test.ok(false,
          "error: candidate has no property matching '" + pkey + "'");
      }
    }
  };

  /**
   */
  var ExactSet = function () {};

  ExactSet.prototype = new MinSet();

  ExactSet.prototype.compare = function (cand, test, keyMap) {
    keyMap = keyMap || {};

    MinSet.prototype.compare.call(this, cand, test, keyMap);

    /* TODO: Remove this check after we remove Prototype. */
    if (!Array.isArray(cand)) {
      var clone = Object.clone(cand);
    
      for (var pkey in this.pattern) {
        if (!this.pattern.hasOwnProperty(pkey)) continue;
        delete clone[keyMap[pkey]];
      }

      for (ckey in clone) {
        if (!clone.hasOwnProperty(ckey)) continue;
        test.ok(false,
          "error: candidate has extra property ('" + ckey + "')");
      }
    }
  }

  var buildAssocPattern = function (Ctor) {
    return function (pattern, keyOrder) {
      var result = new Ctor();
      Min.prototype.initialize.call(result, pattern, keyOrder);
      return result;
    }
  };

  namespace.open("hottest").patterns = {
    Pattern: Pattern,
    key : function (k) { return Key(k); },
    min : buildAssocPattern(Min),
    exact : buildAssocPattern(Exact),
    minSet : buildAssocPattern(MinSet),
    exactSet : buildAssocPattern(ExactSet)
  };

})();

