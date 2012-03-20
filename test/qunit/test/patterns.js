(function () {

  module("patterns");

  test("patterns.Min", function() {
    var P = hottest.patterns;
    var foo = P.min({a: 3, b: 12, c: "hello"});
    foo.compare({a: 3, b: 12, c: "hello"}, QUnit);
    foo.compare({a: 3, b: 12, c: "hello", d: "goodbye"}, QUnit);
    //foo.compare({a: 3, c: "hello"}, QUnit);
    //foo.compare({a: 3, b: 12, c: "goodbye"}, QUnit);
    //foo.compare({a: 3, b: "12", c: "hello"}, QUnit);
    var goo = P.min({
      x: 8,
      y: "howdy",
      z: foo
    });
    goo.compare({a: 3, x: 8, y: "howdy", z: {a: 3, b: 12, c: "hello"}}, QUnit);
    //goo.compare({x: 8, y: "howdy", z: {a: 8, c: "hello"}}, QUnit);
  });

  test("patterns.Exact", function() {
    var P = hottest.patterns;
    var foo = P.exact({a: 3, b: 12, c: "hello"});
    foo.compare({a: 3, b: 12, c: "hello"}, QUnit);
    //foo.compare({a: 3, b: 12, c: "hello", d: "goodbye"}, QUnit);
    //foo.compare({a: 3, c: "hello"}, QUnit);
    //foo.compare({a: 3, b: 12, c: "goodbye"}, QUnit);
    //foo.compare({a: 3, b: "12", c: "hello"}, QUnit);
    var goo = P.exact({
      x: 8,
      y: "howdy",
      z: foo
    });
    //goo.compare({a: 3, x: 8, y: "howdy", z: {a: 3, b: 12, c: "hello"}}, QUnit);
    //goo.compare({x: 8, y: "howdy", z: {a: 8, c: "hello"}}, QUnit);
  });

  test("patterns.MinSet", function () {
    expect(0);
    var P = hottest.patterns;
    var foo = P.minSet(['a', 3, 'goodbye']);
    foo.compare(['a', 3, 'goodbye'], QUnit);
    foo.compare([3, 'goodbye', 'a'], QUnit);
    foo.compare({x: 'goodbye', y: 3, z: 'a'}, QUnit);
    foo.compare(['a', 3, 'goodbye', 'hello'], QUnit);
    //foo.compare([3, 'goodbye'], QUnit);
  });

  test("patterns.ExactSet", function () {
    expect(0);
    var P = hottest.patterns;
    var foo = P.exactSet(['a', 3, 'goodbye']);
    foo.compare(['a', 3, 'goodbye'], QUnit);
    foo.compare([3, 'goodbye', 'a'], QUnit);
    foo.compare({x: 'goodbye', y: 3, z: 'a'}, QUnit);
    foo.compare(['a', 3, 'goodbye', 'hello'], QUnit);
    //foo.compare([3, 'goodbye'], QUnit);
  });

}());

