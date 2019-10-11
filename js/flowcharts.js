poisson = function(mean){
    var L = Math.exp(-mean);
    var p = 1.0;
    var k = 0;

    do {
        k++;
        p *= Math.random();
    } while (p > L);

    return (k - 1);

}

bernoulli = function(p){
    return(Math.random() < p)
}





var treeData = [
  {
    "name": "Top Level",
    "parent": "null",
    "children": [
      {
        "name": "Level 2: A",
        "parent": "Top Level",
        "children": [
          {
            "name": "Son of A",
            "parent": "Level 2: A"
          },
          {
            "name": "Daughter of A",
            "parent": "Level 2: A"
          }
        ]
      },
      {
        "name": "Level 2: B",
        "parent": "Top Level"
      }
    ]
  }
];

var createTreeList= function(r0,generations,vac_coverage){
    if(generations===undefined){
        generations = 3;
    }

    if(vac_coverage===undefined){
        //if vaccine coverage not set then set to 0.
        vac_coverage = 0;
    }
    var arr = [{name: "0", id:"0", vaccinated: false}];
    var id = 0;
    var ngi = [id]; //next gen indices
    for (var gen =0; gen<generations; gen++){
        var ngi_buffer = [];
        for (var i = 0; i < ngi.length; i++ ){
            if(!arr[ngi[i]].vaccinated){ //if not vaccinated then start new chain
                for(var child = 0; child < poisson(r0); child++){ /* start of children */
                    id += 1;
                    arr.push({name: String(id),
                            id: String(id),
                            parent: String(ngi[i]),
                            vaccinated: bernoulli(vac_coverage)
                        });
                    ngi_buffer.push(id);
                } /* end of children */
            }

        } /* end of generation */
        ngi = ngi_buffer.slice();
    }
    return arr;
}

function treeDataGenerator(r0,generations,vac_coverage){



    var arr = createTreeList(r0,generations,vac_coverage);

    // we use a map for search for the nodes by their ids
    var nodes_map = {};

    // we copy all the elements into the map, where the keys are the ids of the nodes
    for ( var i=0; i<arr.length; ++i )
    {
        // create nodes array for each node
        arr[ i ].children = [];

        // copy into the map
        nodes_map[ arr[i].id.toString() ] = arr[ i ];
    }

    // we iterate through all nodes, and add them into their parent's node array
    for ( var key in nodes_map )
    {
        // current node
        var node = nodes_map[ key ];

        // if the current node have idParent property, and the parent exists in the map
        if ( "parent" in node && node.parent.toString() in nodes_map )
        {
            // we add the current node to the parent's nodes array
            nodes_map[ node.parent.toString() ].children.push( node );
        }
    }

    // we remove all the nodes from the map, that has parents
    for ( var key in nodes_map )
    {
        // current node
        var node = nodes_map[ key ];

        // if it has idParent property
        if ( "parent" in node )
        {
            // we remove from the map
            delete nodes_map[ key ];
        }
    }

    // results array
    var new_arr = [];
    // copy back the nodes from the map into an array
    for ( var key in nodes_map )
    {
        new_arr.push( nodes_map[ key ] );
    }
    return new_arr;
}

//var arr = createTreeList(3,3);




function generateTreeDiagram(div,r0,generations,reset,vac_coverage){
    if (reset===undefined){
        reset = false;
    }

    var treeData = treeDataGenerator(r0,generations,vac_coverage);
    // ************** Generate the tree diagram	 *****************
    var margin = {top: 20, right: 120, bottom: 20, left: 120},
    	width = 960 - margin.right - margin.left,
    	height = 500 - margin.top - margin.bottom;

    var i = 0,
    	duration = 750,
    	root;

    var tree = d3.layout.tree()
    	.size([height, width]);

    var diagonal = d3.svg.diagonal()
    	.projection(function(d) { return [d.y, d.x]; });

    var svg;

    if(reset){
        svg = d3.select(div).select("svg");
        svg.selectAll("*").remove();
        svg = svg.append("g")
                 .attr("transform", "translate(" + margin.left + "," + margin.top + ")");
    } else {

        svg = d3.select(div).append("svg")
    	.attr("width", width + margin.right + margin.left)
    	.attr("height", height + margin.top + margin.bottom)
      .append("g")
    	.attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    }

    root = treeData[0];
    root.x0 = height / 2;
    root.y0 = 0;

    update(root);

    d3.select(self.frameElement).style("height", "500px");

    function update(source) {

      // Compute the new tree layout.
      var nodes = tree.nodes(root).reverse(),
    	  links = tree.links(nodes);

      // Normalize for fixed-depth.
      nodes.forEach(function(d) { d.y = d.depth * 180/(generations/3); });

      // Update the nodes…
      var node = svg.selectAll("g.node")
    	  .data(nodes, function(d) { return d.id || (d.id = ++i); });

      // Enter any new nodes at the parent's previous position.
      var nodeEnter = node.enter().append("g")
    	  .attr("class", "node")
    	  .attr("transform", function(d) { return "translate(" + source.y0 + "," + source.x0 + ")"; })
    	  .on("click", click);

      nodeEnter.append("circle")
    	  .attr("r", 1e-6)
    	  .style("fill", function(d) { return d.vaccinated ? "red" : "lightsteelblue"; })
          .style("stroke-width", 2)
          .style("stroke", function(d){ return d.vaccinated ? "red" : "none"; });

      nodeEnter.append("text")
    	  .attr("x", function(d) { return d.children || d._children ? -13 : 13; })
    	  .attr("dy", ".35em")
    	  .attr("text-anchor", function(d) { return d.children || d._children ? "end" : "start"; })
    	  .text(function(d) { return d.name; })
    	  .style("fill-opacity", 1e-6);

      // Transition nodes to their new position.
      var nodeUpdate = node.transition()
    	  .duration(duration)
    	  .attr("transform", function(d) { return "translate(" + d.y + "," + d.x + ")"; });

      nodeUpdate.select("circle")
    	  .attr("r", 10)
    	  .style("fill", function(d) { return d.vaccinated ? "red" : "lightsteelblue"; })
          .style("stroke-width", 2)
          .style("stroke", function(d){ return d.vaccinated ? "red" : "none"; });

      nodeUpdate.select("text")
    	  .style("fill-opacity", 1);

      // Transition exiting nodes to the parent's new position.
      var nodeExit = node.exit().transition()
    	  .duration(duration)
    	  .attr("transform", function(d) { return "translate(" + source.y + "," + source.x + ")"; })
    	  .remove();

      nodeExit.select("circle")
    	  .attr("r", 1e-6);

      nodeExit.select("text")
    	  .style("fill-opacity", 1e-6);

      // Update the links…
      var link = svg.selectAll("path.link")
    	  .data(links, function(d) { return d.target.id; });

      // Enter any new links at the parent's previous position.
      link.enter().insert("path", "g")
    	  .attr("class", "link")
    	  .attr("d", function(d) {
    		var o = {x: source.x0, y: source.y0};
    		return diagonal({source: o, target: o});
    	  });

      // Transition links to their new position.
      link.transition()
    	  .duration(duration)
    	  .attr("d", diagonal);

      // Transition exiting nodes to the parent's new position.
      link.exit().transition()
    	  .duration(duration)
    	  .attr("d", function(d) {
    		var o = {x: source.x, y: source.y};
    		return diagonal({source: o, target: o});
    	  })
    	  .remove();

      // Stash the old positions for transition.
      nodes.forEach(function(d) {
    	d.x0 = d.x;
    	d.y0 = d.y;
      });
    }

    // Toggle children on click.
    function click(d) {
      if (d.children) {
    	d._children = d.children;
    	d.children = null;
      } else {
    	d.children = d._children;
    	d._children = null;
      }
      update(d);
    }
}

generateTreeDiagram("#r0Diagram",1.5,3,false,0);
d3.select("#r0DiagramSimulate").on('click',function(){
    //var r0 = parseFloat($('#r0Selector').val());
    var r0 = document.getElementById("f-inputr0").value/100;
    generateTreeDiagram("#r0Diagram",r0,3,true,0);
});

generateTreeDiagram("#mv-r0Diagram",1.5,3,false,0);
d3.select("#mv-simulate").on('click',function(){
    var r0 = document.getElementById("mv-inputr0").value/100;
    var vac_coverage = document.getElementById("mv-inputvac").value/100;
    var generations = document.getElementById("mv-inputgenerations").value;
    generateTreeDiagram("#mv-r0Diagram",r0,generations,true,vac_coverage);
});

document.getElementById("mv-inputr0").oninput = function() {
  $('#inputr0-label').text('R0: '+(this.value/100).toFixed(2));
}

document.getElementById("f-inputr0").oninput = function() {
  $('#f-inputr0-label').text('R0: '+(this.value/100).toFixed(2));
}

document.getElementById("mv-inputgenerations").oninput = function() {
  $('#inputgenerations-label').text('Generations: '+this.value);
}

document.getElementById("mv-inputvac").oninput = function() {
    $('#inputcoverage-label').text('Vaccination coverage: '+ (this.value/1).toFixed(0) + '%');
}


$(".clickableDiv").click(function() {
  window.location = $(this).data("href");
  return false;
});
/* model fitting introduction */


$(document).ready(function(){
    $('[data-toggle="tooltip"]').tooltip();
    var easter_egg = new Konami(function(){
            window.location.href ='https://www.youtube.com/watch?v=sOnqjkJTMaA#t=4m42s';
    });
});
