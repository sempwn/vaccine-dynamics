// Following this idea: https://hacks.mozilla.org/2011/12/faster-canvas-pixel-manipulation-with-typed-arrays/


function ImageItem(src) {
  this.image = new Image();
  this.image.src = src;
}

function formatDate(d) {
    var month = '' + (d.getMonth() + 1),
        day = '' + d.getDate(),
        year = d.getFullYear(),
        hour = d.getHours(),
        minute = d.getMinutes(),
        second = d.getSeconds();

    if (month.length < 2) month = '0' + month;
    if (day.length < 2) day = '0' + day;
    if (hour.length < 2) hour = '0' + hour;
    if (minute.length < 2) minute = '0' + minute;
    if (second.length < 2) second = '0' + second;

    var result = [year, month, day].join('-');
    result += ' ' + [hour,minute,second].join(':');
    return result;
}

function convertImageArray(img){
    return Array.from(img.data)
}

function copyArray(array,zero){
    var res = [];
    for (var i = 0; i <array.length; i++){
        var row = [];
        for(var j = 0; j < array[0].length; j++){
                if(zero === true)
                    row.push(0);
                else
                    row.push(array[i][j]);

        }
        res.push(row);
    }
    return res;

}

function fixPopulation(S){
    var total_val = 0;
    var pop_size = 4E6;
    for(var i = 0; i < S.length; i ++){
        for(var j = 0; j < S[0].length; j ++){
            total_val += S[i][j];
        }
    }

    var factor = pop_size/total_val;
    for(var i = 0; i < S.length; i ++){
        for(var j = 0; j < S[0].length; j ++){
            S[i][j] = S[i][j]*factor;
        }
    }
    return S;
}

function Simulation(map){
    this.initialise = function(){
        this.S = copyArray(map.dataset);
        //this.S = fixPopulation(this.S);
        this.N = copyArray(map.dataset);
        //this.N = fixPopulation(this.N);
        this.R = copyArray(map.dataset,true);
        this.I = copyArray(map.dataset,true);
        this.statistics = {}; //store statistics such as total infected here.
        this.infLocs = [];
        this.params.intervention = {};
        this.t = 0;
        this.params.vr = 0.;
        this.params.int = 0.;
        this.params.ptravel = 0.;
        for(var i = 0; i < this.S.length; i++){
                for(var j = 0; j < this.S[0].length; j++){
                    if (this.S[i][j] > 1){
                        this.infLocs.push([i,j]);
                    }
                }
        }
        // start an epidemic in Tokyo
        this.I[98][152] +=1;
    }
    this.params = {beta: 1.0, gamma: 1/7.0, tau: 1e-2, imp: 0.01,
                   vr: 0.0, int: 0.0, ptravel: 0.0, sim_speed: 10};
    var R0 = 6; //CHANGED from 8
    this.params.beta = R0*this.params.gamma;
    this.params['intervention'] = {};

    this.graph_data = [{x:[], y:[],type: 'scatter'}];
    this.dt = 0.05;
    this.map = map;
    this.play = false;


    this.initialise();
    //var i = 190,j=500;
    //var i = 100,j=180;
    //var i = 100,j=250;
    this.infLocs = [];
    //this.I[i][j] = 1;
    //this.S[i][j] = this.S[i][j] - this.I[i][j];
    for(var i = 0; i < this.S.length; i++){
            for(var j = 0; j < this.S[0].length; j++){
                if (this.S[i][j] > 1){
                    this.infLocs.push([i,j]);
                }
            }
    }




    function neighbourhoodCalc(array,i,j){
        return array[i+1][j] + array[i-1][j] +
               array[i][j-1] + array[i][j+1] +
               array[i+1][j-1] + array[i+1][j+1] +
               array[i-1][j-1] + array[i-1][j+1] -8*array[i][j];
               -4*array[i][j];
    }

    this.runIntervention = function(intervention){
        var coverage = this.params.intervention[intervention] || 0;
        if(intervention=='vaccinate'){
            this.params.vr = coverage;
        }else if(intervention=='treat'){
            this.params.int = coverage;
        }else if(intervention=='travel'){
            this.params.ptravel = coverage;
        }else{

        }
    }



    this.recordStatistics = function(){
        var infs = 0;
        for(loc in this.infLocs){
            var i = this.infLocs[loc][0],j=this.infLocs[loc][1];
            infs += this.I[i][j];
        }
        this.statistics.infs = infs;
        d3.select('#stats').text('Outbreak of ' + Math.floor(infs) + ' infected');
        //turn time into date
        //var d1 = Date.parse("27 October 2017");
        //var date = formatDate(new Date(d1 + 1000*60*60*24*this.t));
        this.graph_data[0].x.push(this.t); this.graph_data[0].y.push(infs);
        Plotly.extendTraces('graph-plot', {
            x: [[this.t]],
            y: [[infs]]
        },[0]);
        //Plotly.newPlot('graph-plot', this.graph_data);
        //console.log('Infected: ',infs);
    }
    this.importation = function(){
        var loc = Math.floor(Math.random()*this.infLocs.length);
        var inf = this.I[this.infLocs[loc][0]][this.infLocs[loc][1]];
        return inf;
    }
    this.EulerStepSI = function(i,j){
        var p = this.params;
        var infs = this.params.beta*this.S[i][j]*this.I[i][j]/this.N[i][j];
        // Japanese smallpox won't have an importation
        //var importation = (Math.random()<this.dt*1E-4*(1-p.ptravel))? p.imp*this.importation() : 0;
        var dS = -infs * (1 - p.vr);
        var dI = infs * (1-p.vr) - p.gamma*(1/(1-1e-10-p.int))*this.I[i][j] + p.tau*neighbourhoodCalc(this.I,i,j);
        var dR = p.gamma*this.I[i][j];

        return [
                this.S[i][j] + this.dt * dS,
                this.I[i][j] + this.dt * dI,
                this.R[i][j] + this.dt * dR
               ];
    }

    this.stepForward = function(){
        var bS = copyArray(this.S),
            bI = copyArray(this.I),
            bR = copyArray(this.R);
        this.t += this.dt;

        for(loc in this.infLocs){
                var i = this.infLocs[loc][0],j=this.infLocs[loc][1];
                var res = this.EulerStepSI(i,j);
                bS[i][j] = res[0]; bI[i][j] = res[1]; bR[i][j] = res[2];
        }

        this.S = bS; this.I = bI; this.R = bR;
        this.recordStatistics();
    }

    this.drawMap = function(){
        this.map.S = this.S;
        this.map.I = this.I;
        this.map.N = this.N;
        this.map.R = this.R;
        this.map.drawMap();
    }

    this.startSim = function(){
        var obj = this;
        d3.timer(function(){
            if (obj.play){


                obj.stepForward();

                obj.drawMap();
            }
        },500);
    }

    var obj = this;
    d3.select('.mainCanvas').on('click', function() {
        // get mousePositions from the main canvas

        var mouseX = d3.event.layerX || d3.event.offsetX;
        var mouseY = d3.event.layerY || d3.event.offsetY;
        var yy = Math.floor(mouseY/obj.map.yscale),
            xx = Math.floor(mouseX/obj.map.xscale);
        console.log(mouseX,mouseY);
        console.log(yy,xx);
        obj.I[yy][xx] += 1;
    });


}

function colCalc(){
    function hexToRgb(hex) {
        // Expand shorthand form (e.g. "03F") to full form (e.g. "0033FF")
        var shorthandRegex = /^#?([a-f\d])([a-f\d])([a-f\d])$/i;
        hex = hex.replace(shorthandRegex, function(m, r, g, b) {
            return r + r + g + g + b + b;
        });

        var result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
        return result ? {
            r: parseInt(result[1], 16),
            g: parseInt(result[2], 16),
            b: parseInt(result[3], 16)
        } : null;
    }

    var color = d3.scale.linear().domain([1,200])
                .interpolate(d3.interpolateHcl)
                .range([d3.rgb("#007AFF"), d3.rgb('#FFF500')]);
    var arr = [];
    for(var i = 0; i< 500; i++){
        arr.push(hexToRgb(color(i)));
    }
    return arr;
  }
var popColor = colCalc();



function Map(data,map_id){
    function bufferMap(data,width,height){
        //buffers map with zeros.
        var res = [];
        var zerorow = [];

        zerorow.push(0);
        for (var x = 0; x < data[0].length; ++x) {
            zerorow.push(0);
        }
        zerorow.push(0);

        res.push(zerorow);
        for (var y = 0; y < data.length; ++y) {
            var row = [];
            row.push(0);
            for (var x = 0; x < data[0].length; ++x) {
                row.push(data[y][x]);
            }
            row.push(0);
            res.push(row);
        }
        res.push(zerorow);
        return res;
    }
    var chartSize = 1000;
    var dataSize = 1000;

    this.dataset = data;
    this.dataset = bufferMap(this.dataset);
    this.map_id = map_id;
    //var olddataset = d3.range(dataSize).map(function(d, i){return d3.range(dataSize).map(function(d, i){return ~~(Math.random()*255);});});

    this.canvas = d3.select(map_id)
        .append('canvas')
        .style({position: 'relative', width: data[0].length + 'px', height: data.length + 'px'})
        .attr({width: data[0].length, height: data.length})
        .attr('class','mainCanvas')
        .node();

    this.canvasWidth  = this.canvas.width;
    this.canvasHeight = this.canvas.height;
    this.xscale = Math.round($(map_id).parent()[0].clientWidth/data[0].length);
    this.yscale = Math.round($(map_id).parent()[0].clientWidth/data[1].length);

    this.ctx = this.canvas.getContext('2d');
    this.imageData = this.ctx.getImageData(0, 0, this.canvasWidth, this.canvasHeight);

    this.S = copyArray(this.dataset);
    this.N = copyArray(this.dataset);
    this.I = copyArray(this.dataset,true);
    this.R = copyArray(this.dataset,true);

    this.loadImage = function(data){
        this.dataset = data;
        this.dataset = bufferMap(this.dataset);

        d3.select(map_id).select('canvas')
            .attr({width: data[0].length, height: data.length})



        this.canvasWidth  = this.canvas.width;
        this.canvasHeight = this.canvas.height;
        this.ctx = this.canvas.getContext('2d');
        this.imageData = this.ctx.getImageData(0, 0, this.canvasWidth, this.canvasHeight);

        this.S = copyArray(this.dataset);
        this.N = copyArray(this.dataset);
        this.I = copyArray(this.dataset,true);
        this.R = copyArray(this.dataset,true);

    }

    this.drawMap = function(){
        d3.select(map_id).select('canvas')
        .style({ width: (this.canvasWidth*this.xscale) + 'px', height: (this.canvasHeight*this.yscale) + 'px'});

        this.ctx = this.canvas.getContext('2d');
        this.imageData = this.ctx.getImageData(0, 0, this.canvasWidth, this.canvasHeight);


        var buf = new ArrayBuffer(this.imageData.data.length);
        var buf8 = new Uint8ClampedArray(buf);
        var data = new Uint32Array(buf);

        for (var y = 0; y < this.canvasHeight; ++y) {
            for (var x = 0; x < this.canvasWidth; ++x) {
                var yy = Math.floor(y),
                    xx = Math.floor(x);

                var value = Math.floor(this.S[yy][xx]);
                var inf = Math.floor(this.I[yy][xx]);
                var rec = Math.floor(this.R[yy][xx]);
                //orange code: 	(255, 127, 0)
                var ccol = popColor[parseInt(2*inf+2*rec)];
                data[y * this.canvasWidth + x] =
                    (255   << 24) |    // alpha
                    (parseInt(value + ccol.b)  << 16) |    // blue
                    (parseInt(value + ccol.g)  <<  8) |    // green
                     parseInt(value  + ccol.r);            // red
            }
        }

        /* update mouse */

        var xx = this.mouseX || 0;
        var yy = this.mouseY || 0;
        var value = Math.floor(this.S[yy][xx]);
        var inf = Math.floor(this.I[yy][xx]);
        var rec = Math.floor(this.R[yy][xx]);
        //orange code: 	(255, 127, 0)
        var ccol = popColor[parseInt(2*inf+2*rec)];
        d3.select('#tooltip')
            .html(this.S[yy][xx].toFixed(0) + ' susceptible\n' +
                  this.I[yy][xx].toFixed(0) + ' infected\n' +
                  this.R[yy][xx].toFixed(0) + ' recovered\n');//this.I[mouseY][mouseX]);


        this.imageData.data.set(buf8);

        this.ctx.putImageData(this.imageData, 0, 0);
    }
    var obj = this;
    d3.select('.mainCanvas').on('click', function() {
        // get mousePositions from the main canvas

        var mouseX = d3.event.layerX || d3.event.offsetX;
        var mouseY = d3.event.layerY || d3.event.offsetY;
        var yy = Math.floor(MouseY/this.yscale),
            xx = Math.floor(MouseX/this.xscale);
        console.log(mouseX,mouseY);
        obj.I[yy][xx] += 1;
    });
    d3.select('.mainCanvas').on('mousemove', function() {


        // get mousePositions from the main canvas
        var mouseX = d3.event.layerX || d3.event.offsetX;
        var mouseY = d3.event.layerY || d3.event.offsetY;

        if (true) {

            // Show the tooltip only when there is nodeData found by the mouse
            var yy = Math.floor(mouseY/obj.yscale),
                xx = Math.floor(mouseX/obj.xscale);
            obj.mouseX = xx;
            obj.mouseY = yy;
            d3.select('#tooltip')
                .style('opacity', 0.8)
                .style('top', mouseY + 5 + 'px') //d3.event.pageY
                .style('left', mouseX + 5 + 'px') //d3.event.pageX
                .html(Math.floor(obj.S[yy][xx]) + ' susceptible\n' +
                      Math.floor(obj.I[yy][xx]) + ' infected\n' +
                      Math.floor(obj.R[yy][xx])+ ' recovered\n');//this.I[mouseY][mouseX]);
                //console.log(obj.I[mouseY][mouseX]);

        } else {

            // Hide the tooltip when there our mouse doesn't find nodeData

            d3.select('#tooltip')
                .style('opacity', 0);

        }

    }); // canvas listener/handler

    d3.select('.mainCanvas').on('mouseout', function() {
        d3.select('#tooltip')
            .style('opacity', 0);
    });


}

function getBase64Image(img) {
    // Create an empty canvas element
    var canvas = document.createElement("canvas");
    canvas.width = img.width;
    canvas.height = img.height;

    // Copy the image contents to the canvas
    var ctx = canvas.getContext("2d");
    ctx.drawImage(img, 0, 0);
    var imgData = ctx.getImageData(0, 0, canvas.width, canvas.height);
    // Get the data-URL formatted image
    // Firefox supports PNG and JPEG. You could check img.src to
    // guess the original format, but be aware the using "image/jpg"
    // will re-encode the image.
    //var dataURL = canvas.toDataURL("image/png");

    var result = Array.from(imgData.data.values());
    var vecres = [];
    var elem;
    for (var i = 0; i < Math.floor(img.height); i+=2){
        var row = [];
        for (var j = 0; j < Math.floor(img.width); j+=2){
            elem = result[((i * (img.width * 4)) + (j * 4)) + 2];
            if (elem >= 250) elem = 0;
            row.push(elem);

        }
        vecres.push(row);
    }
    return vecres;
}

var backgroundImage = new ImageItem('./img/japan.png');
var imageData,map,sim;
backgroundImage.image.onload = function() {
  imageData = getBase64Image(backgroundImage.image);
  map =  new Map(imageData,'#map');
  map.drawMap();
  sim = new Simulation(map);
  sim.drawMap();
  sim.startSim();
  //addFooter('body');
  //sim.stepForward();
  //sim.drawMap();

}
var areas = {
    "World": {filename:'./img/world.png'},
    "Vancouver": {filename:'./img/lower-mainland.png'},
    "North America": {filename:'./img/north-america.png'}
};
var addFooter = function(div){
    d3.select(div).append('footer')
                     .attr('class','footer')
                     .html(
                    `
                        <hr>
                        <p>© 2017</p>

                        `);
}

d3.select('#area-select').on('change',function(value){
    console.log(this.value);
    var area = areas[this.value];
    var backgroundImage = new ImageItem(area.filename);
    backgroundImage.image.onload = function() {
      imageData = getBase64Image(backgroundImage.image);
      //sim.map.loadImage(imageData);
      //sim.initialise();
      d3.select('#map').selectAll("*").remove();
      map =  new Map(imageData,'#map');
      map.drawMap();
      sim = new Simulation(map);
      sim.drawMap();
      sim.startSim();
    }
});

d3.select('#reset').on('click',function(){
    sim.initialise();
    sim.graph_data = [{x:[], y:[],  mode: 'lines',type: 'scatter'}];
    var layout = {
          title: '',
          xaxis: {
            title: 'Days since start of epidemic'
          },
          yaxis: {
            title: 'No. infected'
          }
        };
    Plotly.newPlot('graph-plot', sim.graph_data,layout);
});

d3.select('#run').on('click',function(){
    if(sim.play){
        d3.select('#run').html('<i class="fa fa-play-circle"></i> Run');
        sim.play = false;
    } else {
        d3.select('#run').html('<i class="fa fa-pause-circle"></i> Pause');
        sim.play = true;
    }
});

d3.selectAll('.btn-intervention').on('click',function(d){
    var intlabel = { 'travel':'ban air travel','vaccinate':'vaccinate population',
                      'treat':'treat infected population' }
    console.log(this.getAttribute("data-int"));
    var intervention = this.getAttribute("data-int");
    if (sim.params.intervention[intervention]){
        range_intslider.value = sim.params.intervention[intervention]*100;
        $("#int-info").html('<div class="alert alert-info" role="alert" >Coverage set to ' +
                            sim.params.intervention[intervention]*100 +
                           '%.</div>');
    } else {
        range_intslider.value = "50";
        $("#int-info").html('');
    }




    d3.select("#begin-intervention").attr("data-int",intervention);
    d3.select("#intModal-title").text("Set intervention: "+ intlabel[intervention]);
    $('#intModal').modal('show');
});

d3.select('#begin-intervention').on('click',function(d){
    var intervention = this.getAttribute("data-int");
    sim.params.intervention[intervention] = range_intslider.value/100;
    sim.runIntervention(intervention);
    console.log(sim.params.intervention);

});


/*
var bslider = $('#beta').slider()
		.on('change', function(){
        var R0 = bslider.getValue();
        $('#beta-label').text('R0: '+R0.toFixed(2));
        sim.params['beta'] =  R0*sim.params['gamma'];
        })
		.data('slider');

var gslider = $('#gamma').slider()
		.on('change', function(){
            var gamma = 1/gslider.getValue();
            $('#gamma-label').text('Infectious period: '+ gslider.getValue().toFixed(2) + ' days');
            sim.params['gamma'] =  gamma;
        })
		.data('slider');
        */



document.getElementById('range-sim-speed').oninput = function() {
  sim.dt = (0.5 - 0.01)*(this.value/100) + 0.01;
}


/*var tauslider = $('#tau').slider()
		.on('change', function(){
        sim.params['tau'] =  Math.pow(2,tauslider.getValue());
        console.log(sim.params['tau']);
        })
		.data('slider');


var intslider = $('#int-slider').slider()
        .on('slide', function(){
            //sim.params.intervention['tau'] = intslider.getValue()/100;
            console.log(intslider.getValue()/100);
        })
        .data('slider');
*/

var range_intslider = document.getElementById('range-int-slider');

range_intslider.oninput = function(){
    $("#int-info").html('<div class="alert alert-info" role="alert" >Coverage set to ' +
                        this.value +
                       '%.</div>');
}

function resetGraph(){
  traceS = {
    x: [],
    y: [],
    name: 'Infected',
    type: 'scatter'
  };
  traceI = {
    x: [],
    y: [],
    name: 'Infected',
    mode: 'lines',
    type: 'scatter'
  };
  t = 0;
  data = [traceS, traceI];
  var layout = {
        title: '',
        xaxis: {
          title: 'Days since start of epidemic'
        },
        yaxis: {
          title: 'No. infected'
      }
      };

  Plotly.plot('graph-plot', data,layout);
}

resetGraph();
$('#modalButton').on('click',function(){
        $('#myModal').modal('toggle');
});
