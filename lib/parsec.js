/**
 * @fileOverview Parser Combinators <p>{@link hotdrink.parser.parsec}</p>
 * @author Wonseok Kim
 */

/**
 * @name hotdrink.parser
 * @namespace
 *   <p>Namespace where parsers reside.</p>
 */

// Modified from Chris Double's JavaScript Parser Combinator
// http://github.com/doublec/jsparse
//
// Copyright (C) 2007 Chris Double.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// 1. Redistributions of source code must retain the above copyright notice,
//    this list of conditions and the following disclaimer.
//
// 2. Redistributions in binary form must reproduce the above copyright notice,
//    this list of conditions and the following disclaimer in the documentation
//    and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,
// INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
// FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
// DEVELOPERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
// PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
// OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
// WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
// ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//

//provides("hotdrink.parser.parsec")

(function() {
  /**
   * @name hotdrink.parser.parsec
   * @namespace
   *   Parser combinators used internally to build expression and Adam parsers.
   */
  var ns = namespace.open("hotdrink.parser.parsec");
  
  /**
   * Position = {line::Integer, col::Integer}
   */
  var Position = Class.create({
    initialize : function (line, col) {
      this.line = line;
      this.col = col;
    },
    toString : function () {
      return "(line " + this.line + ", column " + this.col + ")";
    }
  });
  
  /**
   * State represents the input string with position for next parsing.
   */
  var State = Class.create({
    initialize : function (input, index, pos) {
      this.input = input;
      this.index = index || 0;
      this.length = input.length - this.index;
      this.pos = pos || new Position(1, this.index);
    },
    from : function (index) {
      /* update position */
      var newPos = Object.clone(this.pos);
      var newIndex = this.index + index;
      for (var i = this.index; i < newIndex; ++i) {
        if (this.input.charAt(i) === '\n') {
          newPos.line++; newPos.col = 0;
        } else {
          newPos.col++;
        }
      }
      return new State(this.input, newIndex, newPos);
    },
    substring : function (start, end) {
      return this.input.substring(start + this.index, (end || this.length) + this.index);
    },
    trimLeft : function () {
      var s = this.substring(0);
      var m = s.match(/^\s+/);
      return m ? this.from(m[0].length) : this;
    },
    at : function (index) {
      return this.input.charAt(this.index + index);
    },
    toString : function () {
      return this.substring(0);
    }
  });
  ns.State = State;
  
  /**
   * Message = {pos::Position, unexpected::String, expected::[String]}
   */
  var Message = function (pos, unexpected, expected) {
    this.pos = pos;
    this.unexpected = unexpected;
    this.expected = expected || [];
  };
  
  /**
   * Return object from a parser.
   * 
   * Reply = {consumed::bool, ok::bool, state::State, matched::String, ast::Any, msg::Message}
   */
  var Reply = function (consumed, ok, state, matched, ast, msg) {
    this.consumed = consumed;
    this.ok = ok;
    this.state = state;
    this.matched = matched;
    this.ast = ast;
    this.msg = msg;
  };

  /* returns a new OkReply instance */
  function newOkReply(consumed, state, matched, ast) {
    return new Reply(consumed, true, state, matched, ast);
  };
  
  /* returns a new ErrorReply instance */
  function newErrorReply(consumed, pos, unexpected, expected) {
    return new Reply(consumed, false, null, null, null, new Message(pos, unexpected, expected));
  }
  
  /* pretty printer for error message */
  function printError(msg) {
    var str = "parse error at " + msg.pos.toString() + ": ";
    if (msg.unexpected != "")
      str += "unexpected \"" + msg.unexpected + "\"";
    if (msg.expected != null || msg.expected.length > 0) {
      str += " while expecting " + msg.expected.join(", ");
    }
    return str;
  }
  ns.printError = printError;
  
  
  /**
   * Token parser which consumes a token.
   * @memberOf hotdrink.parser.parsec
   * @param {String} str token to consume
   * @returns {Parser} a parser function
   */
  function token(str) {
    return function (state) {
      var a = state.length >= str.length && state.substring(0, str.length) == str;
      if (a) {
        return newOkReply(true, state.from(str.length), str, str);
      } else {
        var unexpected;
        if (state.length == 0) {
          unexpected = "end of input";
        } else if (state.length < str.length) {
          unexpected = state.substring(0);
        } else {
          unexpected = state.substring(0, str.length);
        }
        return newErrorReply(false, state.pos, unexpected, ["\"" + str + "\""]);
      }
    };
  }
  ns.token = token;
  
  /**
   * Character parser which consumes a character.
   * @memberOf hotdrink.parser.parsec
   */
  function ch(c) {
    return function (state) {
      var a = state.length >= 1 && state.at(0) == c;
      if (a) {
        return newOkReply(true, state.from(1), c, c);
      } else {
        var unexpected;
        if (state.length < 1) {
          unexpected = "end of input";
        } else {
          unexpected = state.at(0);
        }
        return newErrorReply(false, state.pos, unexpected);
      }
    };
  }
  ns.ch = ch;
  
  /**
   * Parser combinator that returns a single character parser
   * (similar to 'ch'). It parses single characters that are in the inclusive
   * range of the 'lower' and 'upper' bounds ("a" to "z" for example).
   * @memberOf hotdrink.parser.parsec
   */
  function range(lower, upper) {
    return function (state) {
      if (state.length < 1) {
        return newErrorReply(false, state.pos, "end of input");
      } else {
        var ch = state.at(0);
        if (ch >= lower && ch <= upper) {
          return newOkReply(true, state.from(1), ch, ch);
        } else {
          return newErrorReply(false, state.pos, ch);
        }
      }
    };
  }
  ns.range = range;
  

  /* Helper function to convert string literals to token parsers
   * and perform other implicit parser conversions.
   */
  function toParser(p) {
    return (typeof (p) == "string") ? token(p) : p;
  }

  /* parser that always fails. */
  function failure(state) {
    return newErrorReply(false, state.pos, "");
  }
  ns.failure = failure;
  
  /**
   * Sequence combinator that processes a number of parsers in sequence.
   * This can take any number of arguments, each one being a parser. 
   * The parser returns succeeds if all the parsers in the sequence
   * succeeds. It fails if any of them fail.
   * <blockquote>
   * ast := [ast(p1), ast(p2), ...]
   * </blockquote>
   * @memberOf hotdrink.parser.parsec
   */
  function seq(/*...*/) {
    var parsers = [];
    for (var i = 0; i < arguments.length; ++i)
      parsers.push(toParser(arguments[i]));
    
    return function (state) {
      var ast = [];
      var matched = "";
      var i;
      var reply = null;
      var consumed = false;
      for (i = 0; i < parsers.length; ++i) {
        var parser = parsers[i];
        reply = parser(state);
        consumed = consumed || reply.consumed;
        if (reply.ok) {
          state = reply.state;
          /* ast is undefined if it should be ignored */
          if (reply.ast != undefined) {
            ast.push(reply.ast);
            matched = matched + reply.matched;
          }
        } else {
          break;
        }
      }
      if (i == parsers.length) {
        return newOkReply(consumed, state, matched, ast);
      } else {
        reply.consumed = consumed;
        return reply;
      }
    };
  }
  ns.seq = seq;

  /**
   * Parser combinator that provides a choice between other parsers.
   * It takes any number of parsers as arguments and returns a parser that will try
   * each of the given parsers in order. The first one that succeeds results in a
   * successfull parse. It fails if all parsers fail.
   * @memberOf hotdrink.parser.parsec
   */
  function choice(/*...*/) {
    var parsers = [];
    for (var i = 0; i < arguments.length; ++i)
      parsers.push(toParser(arguments[i]));

    return function (state) {
      var reply;
      var i;
      var errmsgs = [];
      for (i = 0; i < parsers.length; ++i) {
        /* try alternatives if not consumed */
        var parser = parsers[i];
        reply = parser(state);
        if (reply.ok || reply.consumed) {
          return reply;
        }
        errmsgs.push(reply.msg);
      }
      /* merge error messages; assumes the pos is the same and collect expected ones. */
      var expected = [];
      for (var j=0; j<errmsgs.length; ++j) {
        expected = expected.concat(errmsgs[j].expected);
      }
      return newErrorReply(false, state.pos, state.at(0), expected);
    };
  }
  ns.choice = choice;
  

  /**
   * Parser combinator that looks for zero or more matches of the original parser.
   * It only fails if p fails after consuming input.
   * <blockquote>
   * ast := [ast(p)]
   * <blockquote> 
   * @memberOf hotdrink.parser.parsec
   */
  function many(p, include_ast_always /* = false */) {
    var p = toParser(p);
    include_ast_always = include_ast_always || false;

    return function (state) {
      var ast = [];
      var matched = "";
      var reply;
      var consumed = false;
      while (true) {
        reply = p(state);
        consumed = consumed || reply.consumed;
        if (!reply.ok) break;
        
        ast.push(reply.ast);
        matched = matched + reply.matched;
        if (reply.state.index == state.index) break;
        state = reply.state;
      }
      /* if reply is consumed error, return it */
      if (reply.consumed)
        return reply;
      
      /* if zero match, ast will be ignored by default */
      if (!include_ast_always && ast.length == 0) {
        ast = undefined;
      }
      return newOkReply(consumed, state, matched, ast);
    };
  }
  ns.many = many;

  
  /**
   * Parser combinator that applies the parser p zero or more times, skipping its result. 
   */
  function skipMany(p) {
    var p = toParser(p);

    return function (state) {
      var result;
      while ((result = p(state)).ok) {
        if (result.state.index == state.index) break;
        state = result.state;
      }
      return newOkReply(false, state, "", undefined);
    };
  }
  
  /**
   * Parser combinator that matches zero or one matches of the original parser.
   * It only fails if p fails after consuming input.
   * <blockquote>
   * AST := AST(p) | undefined (if p doesn't match) 
   *        | false (if p doesn't match, but includea_ast_always is true)
   * </blockquote>
   * @memberOf hotdrink.parser.parsec 
   */
  function optional(p, include_ast_always /* = false */) {
    var p = toParser(p);
    include_ast_always = include_ast_always || false;
    
    return function (state) {
      var r = p(state);
      /* Ok reply || Error reply if consumed */
      if (r.ok || r.consumed) {
        return r;
      }
      /* empty reply */
      return newOkReply(false, state, "", include_ast_always ? false : undefined);
    };
  }
  ns.optional = optional;
  
  /**
   * Negate a single character parser. So given 'ch("a")' it will successfully
   * parse any character except for 'a'. Or 'negate(range("a", "z"))' will successfully parse
   * anything except the lowercase characters a-z.
   */
  function negate(p) {
    var p = toParser(p);
    return function (state) {
      var result;

      if (state.length >= 1) {
        var r = p(state);
        if (!r.ok) {
          result = newOkReply(true, state.from(1), state.at(0), state.at(0));
        } else {
          result = newErrorReply(false, state.pos, state.at(0), "");
        }
      }
      else {
        result = newErrorReply(false, state.pos, "end of input", "");
      }
      return result;
    };
  }
  
  /**
   * Character parser that succeeds if the current character is in the supplied list of 
   * characters or ch parsers.
   */
  function oneOf(/*...*/) {
    return choice.apply(null, arguments);
  }
  
  /**
   * Character parser that succeeds if the current character is not in the supplied list of 
   * characters or ch parsers.
   */
  function noneOf(/*...*/) {
    return negate(choice.apply(null, arguments));
  }

  /**
   * Label combinator used to return error message in terms of high-level grammar.
   * Label parser behaves like parser p but when it fails, it sets the expected productions 
   * to the given label.
   * @memberOf hotdrink.parser.parsec
   */
  function label(p, str) {
    var p = toParser(p);
    return function (state) {
      var r = p(state);
      if (r.ok)
        return r;
      r.msg.expected = [str];
      return r;
    };
  }
  ns.label = label;

  /**
   * Parser combinator that passes the AST generated from the parser 'p'
   * to the function 'f'. The result of 'f' is used as the AST in the result.
   * @memberOf hotdrink.parser.parsec
   */
  function action(p, f) {
    var p = toParser(p);
    return function (state) {
      var r = p(state);
      if (r.ok) {
        r.ast = f(r.ast);
      }
      return r;
    };
  }
  ns.action = action;
  
  function join(ast) { 
    return ast.join(""); 
  }
  
  /* Given a parser that produces an array as an ast, returns a
   * parser that produces an ast with the array joined.
   */
  function join_action(p) {
    return action(p, join);
  }

  /* join_seq = join_action(seq(parser1, parser2, ...))
   * seq returns the list of ast, but join_seq returns one joined ast
   */
  function join_seq(/*...*/) {
    return join_action(seq.apply(null, arguments));
  }

  /**
   * Parser combinator that takes one parser. It returns a parser that
   * looks for one or more matches of the original parser.
   * @memberOf hotdrink.parser.parsec
   */
  function many1(p) {
    return action(seq(p, many(p)),
        function (ast) {
          if (ast.length == 1)
            return [ast[0]];
          return [ast[0]].concat(ast[1]);
    });
  }
  ns.many1 = many1;

  /**
   * Parser combinator that ensures that the given parser succeeds but
   * ignores its result. This can be useful for parsing literals that you
   * don't want to appear in the ast.
   * @memberOf hotdrink.parser.parsec
   * @example
   * seq(expect("("), Number, expect(")")) => ast: [Number]
   */
  function expect(p) {
    return action(p, function (ast) {
      return undefined;
    });
  }
  ns.expect = expect;
  
  /**
   * Action which unwraps the single-entry ast produced by seq.
   * @memberOf hotdrink.parser.parsec
   */
  function unwrap(p) {
    return action(p, function (ast) {
      return ast[0];
    });
  }
  ns.unwrap = unwrap;
  
  /* parser that produces an ast which is just matched string. */
  function matched_string(p) {
    return function (state) {
      var r = p(state);
      if (r.ok) {
        r.ast = r.matched;
      }
      return r;
    };
  }


  
  /* parser that consumes whitespaces */
  function simpleSpace (state) {
    var s = state.substring(0);
    var m = s.match(/^\s+/);/* one or more whitespaces */
    if (m) {
      return newOkReply(false, state.from(m[0].length), "", "");
    } else {
      return newErrorReply(false, state.pos, "", "");
    }
  }
  ns.simpleSpace = simpleSpace;

  /* comments */
  var lineTerminator = oneOf(ch("\r"), ch("\n"));
  var noneLineTerminator = noneOf(ch("\r"), ch("\n"));
  var commentLine = token("//");
  var commentStart = token("/*");
  var commentEnd = token("*/");
  var commentEndChars = oneOf(ch("*"), ch("/"));
  var noneCommentEndChars = noneOf(ch("*"), ch("/"));

  var singleLineCommentChars = matched_string(many1(noneLineTerminator));
  var singleLineComment = matched_string(
      seq(commentLine, optional(singleLineCommentChars), expect(lineTerminator)));

  /* inComment does not support nested comment */
  var inComment = function(i) { return inComment(i); };
  var inComment = choice(commentEnd, 
      matched_string(seq(matched_string(many1(noneCommentEndChars)), inComment)), 
      matched_string(seq(commentEndChars, inComment)));
  var multiLineComment = matched_string(seq(commentStart, inComment));

  var whiteSpaceOrComment = skipMany(choice(simpleSpace, singleLineComment, multiLineComment));
  ns.whiteSpaceOrComment = whiteSpaceOrComment;

  /**
   * Parser combinator that skips whitespaces before applying parser.
   * @memberOf hotdrink.parser.parsec
   */
  /*
  function whitespace (p) {
    var p = toParser(p);
    return function (state) {
      var result;
      result = p(state.trimLeft());
      return result;
    };
  }*/
  function whitespace (p) {
    return unwrap(seq(whiteSpaceOrComment, p));
  }
  ns.whitespace = whitespace;

  /**
   * Sequence combinator which also ignores whitespace between individual parsers.
   * @memberOf hotdrink.parser.parsec
   */
  function wseq (/*...*/) {
    var parsers = [];
    for (var i = 0; i < arguments.length; ++i) {
      parsers.push(whitespace(toParser(arguments[i])));
    }
    return seq.apply(null, parsers);
  }
  ns.wseq = wseq;

  /**
   * Parser which parses inside parenthesis.
   * if include_parens is true, AST will be ["(", AST(p), ")"].
   * @memberOf hotdrink.parser.parsec
   */
  function parens(p, include_parens /* = false */) {
    var p = toParser(p);
    include_parens = include_parens || false;
    return action( wseq("(", p, ")"), function (ast) {
      if (include_parens) return ast;
      else return ast[1];
    });
  }
  ns.parens = parens;

  function chain(p, sep, f) {
    var p = toParser(p);

    return action(seq(p, many(action(seq(sep, p), f), true)), function (ast) {
      return [ ast[0] ].concat(ast[1]);
    });
  }
  
  /**
   * Parser that matches lists of things. The parser to
   * match the list item and the parser to match the seperator need to
   * be provided. The AST is the array of matched items. 
   * @memberOf hotdrink.parser.parsec
   */
  function list(p, sep) {
    return chain(p, sep, function (ast) {
      return ast[1];
    });
  }
  ns.list = list;

  /**
   * Like list, but ignores whitespace between individual parsers. 
   * @memberOf hotdrink.parser.parsec
   */
  function wlist(/*parser, separator*/) {
    var parsers = [];
    for (var i = 0; i < arguments.length; ++i) {
      parsers.push(whitespace(arguments[i]));
    }
    return list.apply(null, parsers);
  }
  ns.wlist = wlist;
  
  
  /* Common parsers */

  /**
   * @memberOf hotdrink.parser.parsec
   */
  var booleanLiteral = choice("true", "false");

  var zero = token("0");
  var hexDigit = choice(range("0", "9"), range("a", "f"), range("A", "F"));
  var hexIntegerLiteral = join_seq(choice("0x", "0X"), many1(hexDigit));
  var decimalDigit = range("0", "9");
  var nonZeroDigit = range("1", "9");
  var decimalDigits = matched_string(many1(decimalDigit)); 
  var decimalIntegerLiteral = choice(zero, join_seq(nonZeroDigit, optional(decimalDigits)));
  var signedInteger = choice(decimalDigits, seq("+", decimalDigits), seq("-", decimalDigits));
  var exponentIndicator = choice("e", "E");
  var exponentPart = join_seq(exponentIndicator, signedInteger);
  var decimalLiteral = matched_string(choice(
      seq(decimalIntegerLiteral, choice(
          seq(".", optional(decimalDigits), optional(exponentPart)), 
          optional(exponentPart) 
      )),
      seq(".", decimalDigits, optional(exponentPart)))
      );

  /**
   * @memberOf hotdrink.parser.parsec
   */
  var numericLiteral = choice(hexIntegerLiteral, decimalLiteral);
  
  var singleEscapeCharacter = choice("'", "\"", "\\", "b", "f", "n", "r", "t", "v");
  var nonEscapeCharacter = negate(singleEscapeCharacter);

  var characterEscapeSequence = choice(singleEscapeCharacter, nonEscapeCharacter);
  var hexEscapeSequence = seq("x", hexDigit, hexDigit);
  var unicodeEscapeSequence = seq("u", hexDigit, hexDigit, hexDigit, hexDigit);
  var escapeSequence = choice(hexEscapeSequence, unicodeEscapeSequence, characterEscapeSequence);
  var singleStringCharacter = choice(negate(choice("\'", "\\", "\r", "\n")),
                                     seq("\\", escapeSequence));
  var doubleStringCharacter = choice(negate(choice("\"", "\\", "\r", "\n")),
                                     seq("\\", escapeSequence));
  var singleStringCharacters = matched_string(many1(singleStringCharacter));
  var doubleStringCharacters = matched_string(many1(doubleStringCharacter));

  /**
   * @memberOf hotdrink.parser.parsec
   */
  var stringLiteral = matched_string(
        choice(seq("\"", optional(doubleStringCharacters), "\""),
        seq("'", optional(singleStringCharacters), "'")));

  /**
   * @memberOf hotdrink.parser.parsec
   */
  var literal = label(choice(booleanLiteral, numericLiteral, stringLiteral), "literal");
  
  var identifierLetter = choice(range("a", "z"), range("A", "Z"));
  var identifierStart = choice(identifierLetter, "$", "_");
  var identifierPart = choice(identifierStart, decimalDigit);
  /**
   * @memberOf hotdrink.parser.parsec
   */
  var identifier = label(matched_string(seq(identifierStart, many(identifierPart))), "identifier");

  /**
   * @function
   * @memberOf hotdrink.parser.parsec
   */
  var comment = choice(singleLineComment, multiLineComment);
  
  ns.booleanLiteral = booleanLiteral;
  ns.numericLiteral = numericLiteral;
  ns.stringLiteral = stringLiteral;
  ns.literal = literal;
  ns.identifier = identifier;
  ns.comment = comment;
  
})();

