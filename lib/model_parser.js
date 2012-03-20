/**
 * @fileOverview Adam Parser <p>{@link hotdrink.parser.ModelParser}</p>
 * @author Wonseok Kim
 */

//provides("hotdrink.parser.ModelParser");

//requires("hotdrink.parser.parsec");
//requires("hotdrink.parser.ExprParser");

(function () {
  /**
   * @name hotdrink.parser.ModelParser
   * @class Adam Parser.
   */
  var ns = namespace.open("hotdrink.parser.ModelParser");
  
  /* import */
  var parsec = namespace.open("hotdrink.parser.parsec");
  var ExprParser = namespace.open("hotdrink.parser.ExprParser");
  var token = parsec.token;
  var choice = parsec.choice;
  var seq = parsec.seq;
  var wseq = parsec.wseq;
  var optional = parsec.optional;
  var many = parsec.many;
  var many1 = parsec.many1;
  var action = parsec.action;
  var expect = parsec.expect;
  var identifier = parsec.identifier;
  var wlist = parsec.wlist;
  var parens = parsec.parens;
  var expression = ExprParser.expression;
  var unwrap = parsec.unwrap;
  var whiteSpaceOrComment = parsec.whiteSpaceOrComment;
  var label = parsec.label;


  /* the type constants of expression AST */
  var Type = {
    INPUT: "input",
    CONSTANT: "constant",
    INTERFAZE: "interfaze",
    LOGIC: "logic",
    OUTPUT: "output",
    INVARIANT: "invariant",
    VAR_DECL: "var_decl",
    RELATE: "relate"
  };

  /* the type node of expression AST */
  var Node = function (type, name, data) {
    this.type = type;
    this.name = name || "";
    this.data = data;
  };
  
  var Sheet = function (id) {
    this.id = id;
    this.input = [];
    this.constant = [];
    this.interfaze = [];
    this.logic = [];
    this.output = [];
    this.invariant = [];
  };
  
  
  var endStatement = token(";");
  
  var initializer = unwrap(wseq(expect(":"), expression));
  var defineStatement = unwrap(wseq(expect("<=="), expression));
  
  var namedDecl = action(
      wseq(wlist(identifier, ","), defineStatement, expect(endStatement)),
      function (ast) {
        var ids = ast[0];
        var expr = ast[1];
        return new Node(Type.VAR_DECL, ids, expr);
      });
  
  var relateExpression = namedDecl;
    
  var relateDecl = action(
      wseq("relate", "{", many1(relateExpression), "}"),
      function (ast) {
        var exprs = ast[2];
        return new Node(Type.RELATE, "", exprs);
      });
  
  /* source-variable-decl */
  var sourceCell = action(
      wseq(identifier, optional(initializer, true), expect(endStatement)),
      function (ast) {
        var id = [ast[0]];
        var expr = ast[1];/* false if none */
        return new Node(Type.VAR_DECL, id, expr === false ? null : expr);
      });
  
  /* constant-variable-decl */
  var constantCell = action(
      wseq(identifier, initializer, expect(endStatement)),
      function (ast) {
        var id = [ast[0]];
        var expr = ast[1];
        return new Node(Type.VAR_DECL, id, expr);
      });

  /* note: relateDecl should be applied first (restricted lookahead) */
  var logicCell = choice(relateDecl, namedDecl);
  var outputCell = namedDecl;
  
  function section(type, p) {
    return action(p, function (ast) {
      var cells = ast[3];/* [] if none */
      return new Node(type, "", cells);
    });
  }
  
  var inputDecl = section(Type.INPUT, 
      wseq("input", ":", "{", many(sourceCell, true), "}"));
      
  var constantDecl = section(Type.CONSTANT, 
      wseq("constant", ":", "{", many(constantCell, true), "}"));
  
  var interfaceDecl = section(Type.INTERFAZE,
      wseq("interface", ":", "{", many(sourceCell, true), "}"));
  
  var logicDecl = section(Type.LOGIC,
      wseq("logic", ":", "{", many(logicCell, true), "}"));
  
  var outputDecl = section(Type.OUTPUT,
      wseq("output", ":", "{", many(outputCell, true), "}"));
  
  var invariantDecl = section(Type.INVARIANT,
      wseq("invariant", ":", "{", many(outputCell, true), "}"));
  
  
  var sectionDecl = choice(inputDecl, constantDecl, interfaceDecl, logicDecl, 
      outputDecl, invariantDecl);
  
  var sheetDecl = action(
      wseq(expect("sheet"), optional(identifier, true), expect("{"), many(sectionDecl, true), expect("}")),
      function (ast) {
        var id = ast[0];
        
        var sheet = new Sheet(id);
        var sections = ast[1];
        
        /* collect statements(cells) */
        for (var i=0; i<sections.length; i++) {
          for (var j=0; j<sections[i].data.length; j++) {
            sheet[sections[i].type].push(sections[i].data[j]);
          }
        }
        return sheet;
      });
  
  var sheetParser = unwrap(seq(sheetDecl, whiteSpaceOrComment));
  
  
  var CELL_TYPE = {
    input : "input",
    constant : "constant",
    interfaze : "interface",
    logic : "logic",
    output : "output",
    invariant : "invariant"
  };
  
  var Variable = function (cellType, initExpr) {
    this.cellType = cellType ? cellType : null;
    this.usedBy = [];
    if (initExpr) {
      this.initExpr = initExpr;
    }
  };
  
  var Method = function (inputs, outputs) {
    this.inputs = inputs;
    this.outputs = outputs;
  };
  
  var Constraint = function (methods) {
    this.methods = methods;
  };
  
  var CGraph = function () {
    this.variables = {};
    this.methods = {};
    this.constraints = {};
  };
  
  /* Internal graph which stores all data about cgraph */
  var WGraph = Class.create({
    initialize : function () {
      this.variables = new Hash();
      this.methods = new Hash();
      this.methodDefs = new Hash();
      this.constraints = new Hash();
      this.errors = [];
      
      this.methodCount = 0;
      this.constraintCount = 0;
    },
    getVariable : function (varId) {
      return this.variables.get(varId);
    },
    setVariable : function (varId, v) {
      this.variables.set(varId, v);
    },
    getNewMethodId : function () {
      var s = "__method_" + (++this.methodCount);
      return s;
    },
    addMethod : function (methodId, method, code) {
      this.methods.set(methodId, method);
      this.methodDefs.set(methodId, code);
    },
    getNewConstraintId : function () {
      var s = "__constraint_" + (++this.constraintCount);
      return s;
    },
    addConstraint : function (constraint) {
      var cnId = this.getNewConstraintId();
      this.constraints.set(cnId, constraint);
      return cnId;
    },
    toGraph : function () {
      if (this.errors.length > 0) {
        var err = "error in model: " + this.errors.join("; \n");
        throw err;
      }
      
      var graph = new CGraph();
      var i;
      this.variables.each(function (pair) {
        graph.variables[pair.key] = pair.value;
      });
      this.methods.each(function (pair) {
        graph.methods[pair.key] = pair.value;
      });
      this.constraints.each(function (pair) {
        graph.constraints[pair.key] = pair.value;
      });
      var defs = [];
      this.methodDefs.each(function (pair) {
        defs.push(pair.key + " : " + pair.value);
      });
      var code = "{\n" + defs.join(",\n") + "\n}";
      
      var obj = {cgraph: graph, methods: code};
      return obj; 
    }
  });

  function undefinedVarError(vid, sectionType) {
    return "Undefined variable \"" + vid + "\" in " + sectionType + " section";
  }
  
  function redefiningVarError(vid, sectionType) {
    return "Redefining variable \"" + vid + "\" in " + sectionType + " section";
  }
  
  /*
   * Process each section.
   */
  var processSection = function (graph, cells, sectionType, defineNewVariable,  
      useExistingVariable, useInitializer, createConstraint) {

    var methods = [];
    for (var i=0; i<cells.length; ++i) {
      var cell = cells[i];
      
      /* declare a new variable */
      if (cell.type === Type.VAR_DECL) {
        var vids = cell.name;
        var expr = cell.data;
        
        for (var j=0; j<vids.length; ++j) {
          var vid = vids[j];
          var v = null;

          /* check if already exists */
          v = graph.getVariable(vid);
          if (defineNewVariable && v != null) {
            //TODO add position info
            graph.errors.push(redefiningVarError(vid, sectionType));
          }
          
          if (v == null) {
            if (useExistingVariable) {
              graph.errors.push(undefinedVarError(vid, sectionType));
            } else {
              /* input, constant, or interface variables can have initializer */
              var initExpr = null;
              if (expr !== null && useInitializer) {
                initExpr = transformInitializer(expr);
              }
  
              v = new Variable(sectionType, initExpr);
              graph.setVariable(vid, v);
            }
          }
        }
        
        if (!useInitializer) {
          /* define a method */
          var methodId = graph.getNewMethodId();
          var outputs = vids;
          /* TODO: impossible to know whether the unknown is undefined or foreign
           * used has only known variables for now.
           */
          var t = transformMethod(graph, outputs, expr);
          var used = t.used;
          /* update usedBy and check variables */
          for (var k=0; k<used.length; ++k) {
            var usedv = graph.getVariable(used[k]);
            if (usedv == null) {
              graph.errors.push(undefinedVarError(used[k], sectionType));
            } else {
              /* v is used by this method */
              if (!contains(usedv.usedBy, methodId))
                usedv.usedBy.push(methodId);
            }
          }
          
          var inputs = t.inputs;
          var def = t.code;
          var method = new Method(inputs, outputs);
          graph.addMethod(methodId, method, def);
          
          /* constraint for each method */
          if (createConstraint) {
            var cn = new Constraint([methodId]);
            graph.addConstraint(cn);
          } else {
            methods.push(methodId);
          }
        }
      } else if (cell.type == Type.RELATE) {
        var c_methods = processSection(graph, cell.data, sectionType, false, true, false, false);
        var cn = new Constraint(c_methods);
        graph.addConstraint(cn);
      }
    }
    return methods;
  };

  /* code transformer for initializer expressions,
   * which replaces var_ref with model.get(id) */
  var InitializerWriter = Class.create(ExprParser.DefaultCodeVisitor, {
    /* TODO: check whether variable is defined */
    visitVarRef : function (node) {
      /* assume that variable is defined as input */
      return "model.get(\"" + node.id + "\")";
    }
  });
  var initWriter = new InitializerWriter();

  /* transformInitializer :: exprAST -> code */
  var transformInitializer = function (expr) {
    return initWriter.visit(expr);
  };
  
  var MethodWriter = Class.create(ExprParser.DefaultCodeVisitor, {
    initialize : function (graph, outputs) {
      this.graph = graph;
      this.outputs = outputs; /* output variables */
      this.used = [];         /* collected variables */
      this.inputs = [];       /* collected input variables */ 
    },
    visitVarRef : function (node) {
      var vid = node.id;
      
      /* assume that unknown variable is foreign variable */
      if (this.graph.getVariable(vid) == null)
        return vid;
      
      /* collect input variables as well */
      if (!contains(this.used, vid))
        this.used.push(vid);

      var code = "model.eval(\"" + vid + "\")";
      if (!contains(this.inputs, vid))
        this.inputs.push(vid);
      
      return code;
    }
  });
  
  /* transform the expression into function definition */
  var transformMethod = function (graph, outputs, expr) {
    /* return function definition code */
    var writer = new MethodWriter(graph, outputs);
    var code = writer.visit(expr);
    var functionDef = "function (model) {return (" + code + ");}";
    return {code: functionDef, used: writer.used, inputs: writer.inputs};
  };
  
  var constructGraph = function (sheet) {
    var graph = new WGraph();
    
    processSection(graph, sheet.input, CELL_TYPE.input, true, false, true, false);
    processSection(graph, sheet.constant, CELL_TYPE.constant, true, false, true, false);
    processSection(graph, sheet.interfaze, CELL_TYPE.interfaze, true, false, true, false);
    processSection(graph, sheet.logic, CELL_TYPE.logic, false, false, false, true);
    processSection(graph, sheet.output, CELL_TYPE.output, true, false, false, true);
    processSection(graph, sheet.invariant, CELL_TYPE.invariant, true, false, false, true);
    
    return graph.toGraph();
  };
  
  /**
   * Parse sheet description and generate a constraint graph.
   * If there is error in the sheet, this will throw an exception with error message.
   * @memberOf hotdrink.parser.ModelParser
   * 
   * @param {String} input Sheet description in Adam.
   * @returns {Object} Object which has cgraph and methods definitions
   */
  var parse = function (input) {
    var state = new parsec.State(input);
    
    var reply = sheetParser(state);
    if (!reply.ok) {
      var err = parsec.printError(reply.msg);
      throw err;
    }
    DEBUG_BEGIN;
    /* if input is not consumed completely, warn the invalid trailing characters */
    if (reply.state.length > 0) {
      ERROR("Invalid trailing string: " + reply.state.toString());
    }
    DEBUG_END;
    
    var sheet = reply.ast;
    LOG("sheet AST:\n" + sheetToString(sheet));
    
    var graph = constructGraph(sheet);
    LOG("parser output:\n" + graphToString(graph));
    
    return graph;
  };
  ns.parse = parse;
 
  DEBUG_BEGIN;
  /* helpers to print sheet and graph */
  
  var cellsToString = function (cells, level) {
    var s = "";
    var indent = "";
    for (var i=0; i<level; ++i) {
      indent += "  ";
    }
    for (var i=0; i<cells.length; ++i) {
      var c = cells[i];
      s += indent;
      if (c.type === Type.VAR_DECL) {
        s += c.name;
        var e = c.data;
        if (e != null) {
          s += " = " + ExprParser.writeJavaScript(e);
        }
        s += "\n";
      } else if (c.type === Type.RELATE) {
        s += "relate:\n";
        s += cellsToString(c.data, level+1);
      }
    }
    return s;
  };

  var sheetToString = function (sheet) {
    var s = "sheet " + sheet.id + "\n";
    s += "  input:\n";
    s += cellsToString(sheet.input, 2);
    s += "  constant:\n";
    s += cellsToString(sheet.constant, 2);
    s += "  interface:\n";
    s += cellsToString(sheet.interfaze, 2);
    s += "  logic:\n";
    s += cellsToString(sheet.logic, 2);
    s += "  output:\n";
    s += cellsToString(sheet.output, 2);
    s += "  invariant:\n";
    s += cellsToString(sheet.invariant, 2);
    
    return s;
  };
  
  var graphToString = function (graph) {
    var s = "cgraph:\n";
    var cgraph = graph.cgraph;
    s += "  variables:\n";
    $H(cgraph.variables).each(function (pair) {
      s += "    " + pair.key + " : " + Object.toJSON(pair.value) + "\n"; 
    });
    s += "  methods:\n";
    $H(cgraph.methods).each(function (pair) {
      s += "    " + pair.key + " : " + Object.toJSON(pair.value) + "\n";
    });
    s += "  constraints:\n";
    $H(cgraph.constraints).each(function (pair) {
      s += "    " + pair.key + " : " + Object.toJSON(pair.value) + "\n";
    });
    
    s += "methods: \"" + graph.methods + "\"";
    return s;
  };
  DEBUG_END;
  
})();
