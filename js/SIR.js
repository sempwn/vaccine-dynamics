var params = {R0: 2, gamma: 1/7, S: 100000, I: 10, R: 0, dt: 0.05};
var sparams = {R0: 2, gamma: 1/7, S: 100000, I: 10, R: 0, dt: 1.0};
var maxT = 150; //100 days
var nreps = 20; //number of replicates.
var data = {}; //data to fit against.
data.I = [1,1,2,2,2,3,4,3,4,6,6,6,8,9,13,11,9,11,12,14,20,19,17,24,19,18,23,24,24,28,32,39,42,49,56,59,58,68,67,77,95,100,106,122,126,123,
    125,142,177,201,196,218,224,234,231,268,289,290,318,336,376,405,406,442,458,464,468,503,522,547,542,550,557,550,515,519,534,552,549,555,
    546,558,549,531,520,490,469,459,467,444,433,421,418,399,388,370,354,337,342,327,332,306,297,273,263,240,233,233,225,209,197,189,183,171,
    149,134,123,103,94,90,90,93,90,82,81,75,65,53,46,41,40,41,35,31,32,31,28,26,24,20,18,17,12,11,8,6,7,7,7,6,6];
data.t = [];
for(var i=0; i < data.I.length; i++){
    data.t.push(i);
}
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

simpleSIR = function(params,T){
  var beta = params.R0 * params.gamma;
  var gamma = params.gamma;
  var coverage = (params.coverage)? params.coverage : 0;
  var p = (1 - coverage); //prob uninfected
  var dt = params.dt;
  var S0 = p*params.S;
  var V0 = (1 - p)*params.S;
  var I0 = params.I;
  var R0 = params.R;
  var N = V0 + S0 + I0 + R0;
  var Is = [I0];
  var Ss = [S0];
  var Rs = [R0];
  var ts = [0];
  var S=S0; var I=I0; var R=R0; var t=0;
  for(var i = 0; i < T/dt; i++){
    dS = -beta*S*I/N;
    dR = gamma*I;
    dI = beta*S*I/N - gamma*I;

    S = S + dt*dS;
    R = R + dt*dR;
    I = I + dt*dI;
    t = t + dt;

    Is.push(I);
    Rs.push(R);
    Ss.push(S);
    ts.push(t);
  }
  return {S:Ss,I:Is,R:Rs,V:V0,
      t:ts}
}

frequencySIR = function(params,T){
  var beta = params.R0 * params.gamma;
  var gamma = params.gamma;
  var dt = params.dt;
  var S0 = params.S;
  var I0 = params.I;
  var R0 = params.R;
  var N = S0 + I0 + R0;
  var Is = [I0];
  var Ss = [S0];
  var Rs = [R0];
  var ts = [0];
  var S=S0; var I=I0; var R=R0; var t=0;
  for(var i = 0; i < T/dt; i++){
    dS = -beta*S*I;
    dR = gamma*I;
    dI = beta*S*I - gamma*I;

    S = S + dt*dS;
    R = R + dt*dR;
    I = I + dt*dI;
    t = t + dt;

    Is.push(I);
    Rs.push(R);
    Ss.push(S);
    ts.push(t);
  }
  return {S:Ss,I:Is,R:Rs,t:ts}
}

vectorSIR = function(params,T){
  var r = params.gamma;
  var a = 1, b = 1/10., c = 5., g = 1, nu = 0.5;
  var tau = a*a*b*c*Math.exp(-g*nu)/g;

  var m = params.R0 * r/tau;

  var dt = params.dt;
  var S0 = params.S;
  var I0 = params.I;
  var Z0 = 0;
  var N = S0 + I0;

  I0 = I0/N;
  S0 = S0/N;

  var Is = [N*I0];
  var Zs = [Z0];
  var ts = [0];

  var I=I0; var S=S0; var Z=Z0; var t=0;
  for(var i = 0; i < T/dt; i++){
    dS = -m*a*b*Z*S;
    dI = m*a*b*Z*S - r*I;
    dZ = a*c*I*(Math.exp(-g*nu) - Z) - g*Z;

    Z = Z + dt*dZ;
    S = S + dt*dS;
    I = I + dt*dI;
    t = t + dt;

    Is.push(N*I);
    Zs.push(Z);
    ts.push(t);
  }
  return {I:Is,Z:Zs,t:ts}
}

SEIR = function(params,T){
    var beta = params.R0 * params.gamma;
    var gamma = params.gamma;
    var dt = 0.01;
    var k = 1/20.;
    var S0 = params.S;
    var I0 = params.I;
    var E0 = 0;
    var R0 = params.R;
    var N = S0 + I0 + R0;
    var Is = [I0];
    var Ss = [S0];
    var Es = [E0];
    var Rs = [R0];
    var ts = [0];
    var S=S0; var E = E0; var I=I0; var R=R0; var t=0;
    for(var i = 0; i < T/dt; i++){
      dS = -beta*S*I/N;
      dE = beta*S*I/N - k*E;
      dI = k*E - gamma*I;
      dR = gamma*I;

      S = S + dt*dS;
      I = I + dt*dI;
      E = E + dt*dE;
      R = R + dt*dR;

      t = t + dt;

      Is.push(I);
      Rs.push(R);
      Ss.push(S);
      Es.push(E);
      ts.push(t);
    }
    return {S:Ss,I:Is,E:Es,R:Rs,t:ts}
}

multipleStochsticSIR = function(params,T,n){
  Is = [];
  for (var i = 0; i < n; i++){
    res = simpleStochasticSIR(params,T);
    Is.push(res.I);
  }
  return {I:Is,t:res.t}
}

simpleStochasticSIR = function(sparams,T,genData){
 if(genData===undefined){
     genData = false;
 }
  var beta = sparams.R0 * sparams.gamma;
  var gamma = sparams.gamma;
  var dt = params.dt;
  var S0 = sparams.S;
  var I0 = sparams.I;
  var R0 = sparams.R;
  var N = S0 + I0 + R0;
  var Is = [I0];
  var Ss = [S0];
  var Rs = [R0];
  var ts = [0];
  var S=S0; var I=I0; var R=R0; var t=0;
  for(var i = 0; i < T/dt; i++){
    infected = poisson(dt*beta*S*I/N);
    recovered = poisson(dt*gamma*I);


    S = S - infected;
    R = R + recovered;
    I = I + infected - recovered;
    t = t + dt;

    S = (S<0)? 0:S;
    I = (I<0)? 0:I;
    R = (R<0)? 0:R;
    if(!genData || (i % Math.floor(1/dt)) ){
        Is.push(I);
        Rs.push(R);
        Ss.push(S);
        ts.push(t);
    }

  }
  if(genData){
      var printOut = '[';
      for(var i = 0; i<Is.length; i++){
          printOut += Is[i]+',';
      }
      printOut += ']';
      console.log(printOut);

  }
  return {S:Ss,I:Is,R:Rs,t:ts}
}


var pause = false;


var counterfactualParam = function(params){
    var counter = Object.assign({}, params);
    counter.coverage = 0;
    return counter;
}

















function plotGraph(div,res,res_counter){
  traceI = {
    x: res.t,
    y: res.I,
    name: 'model',
    type: 'scatter'
  };


  /*
  traceData = {
    x: data.t,
    y: data.I,
    name: 'data',
    mode: 'markers',
    type: 'scatter'
  };
  */

  plotData = [traceI];

  if(res_counter){
      var ctrace = {
        x: res_counter.t,
        y: res_counter.I,
        name: 'no intervention',
        type: 'scatter'
      };
      plotData.push(ctrace);
  }

  var layout = { //main layout format for plot.ly chart
    autosize: true,
    showlegend: true,
    title: 'Epidemic curve',
    hovermode: 'closest',
    textposition: 'top right',
    xaxis: {
      title: 'time since start of epidemic (days)',
      showgrid: true,
      zeroline: false
    },
    yaxis: {
      title: 'number of individuals',
      showline: false,
      rangemode: 'tozero',
      autorange: true,
      zeroline: true
    }
  };

  Plotly.newPlot(div, plotData, layout);
}

function plotTotalGraph(div,res,cres){
    var x = ['no intervention','intervention'];

    var traceV = {
      x: x,
      y: [cres.V, res.V],
      name: 'vaccinated',
      type: 'bar'
    };

    var traceS = {
      x: x,
      y: [cres.S[cres.S.length - 1], res.S[res.S.length - 1]],
      name: 'susceptible',
      type: 'bar'
    };

    var traceI = {
      x: x,
      y: [cres.R[cres.R.length - 1] + cres.I[cres.I.length - 1],
            res.R[res.R.length - 1] + res.I[res.I.length - 1]],
      name: 'infected',
      type: 'bar'
    };

    var data = [traceV,traceS, traceI];

    var layout = {barmode: 'stack'};

    Plotly.newPlot(div, data, layout);
}

function plotStochGraph(div,res){
  var data = [];
  for (i=0; i<res.I.length;i++){
    trace = {
      x: res.t,
      y: res.I[i],
      mode: 'lines',
      hoverinfo: 'none',
      type: 'scatter',
      line: {
        color: 'rgb(200, 200, 200)',
        width: 1,
        opacity: 0.8
      }
    };
    data.push(trace);
  }

  var layout = { //main layout format for plot.ly chart
    autosize: true,
    showlegend: true,
    title: 'stochastic epidemic',
    hovermode: 'closest',
    textposition: 'top right',
    xaxis: {
      title: 'time since start of epidemic (days)',
      showgrid: true,
      zeroline: false
    },
    yaxis: {
      title: 'number of individuals',
      showline: false,
      rangemode: 'tozero',
      autorange: true,
      zeroline: true
    }
  };

  Plotly.newPlot(div, data,layout);
}


function plotAllGraphs(){
    var counter = counterfactualParam(params);
    counterOutput = counter.model(counter,maxT);
    modelOutput = params.model(params,maxT);
    plotGraph('SIRGraphDiv',modelOutput,counterOutput);
    plotTotalGraph('totalGraphDiv',modelOutput,counterOutput);
}




var bestScore = {};
bestScore['direct-contact'] = {mse:1/0,R0:0,gamma:0,model:''};
bestScore['vector-borne'] = {mse:1/0,R0:0,gamma:0,model:''};
bestScore['latent-stage'] = {mse:1/0,R0:0,gamma:0,model:''};
function updateStatistics(params,modelOutput){
    var mse = 0;
    for(var i = 0; i < modelOutput.I.length; i++){
        if(i%Math.floor(1/params.dt)==0){
            mse += Math.pow(modelOutput.I[i] - data.I[Math.floor(i*params.dt)],2);
        }
    }

    mse *= 1/data.I.length;
    if(mse <= bestScore[params.modelName].mse){
        bestScore[params.modelName].mse = mse;
        bestScore[params.modelName].R0 = params.R0;
        bestScore[params.modelName].gamma = params.gamma;
        bestScore[params.modelName].modelName = params.modelName;
    }
    var table = '';
    table+= `  <thead>
                <tr data-toggle="collapse" data-target=".accordion" class="clickable">
                  <th></th>
                  <th>Model name</th>
                  <th>R0</th>
                  <th>Infectious period (days)</th>
                  <th>Mean squared error</th>
                </tr>
              </thead>`;
    table += '<tbody> <tr class="accordion collapse">';
    table += '<th>Current</th>  <th>' + params.modelName + '</th>' +
             '<th>' + params.R0 + '</th>' +
             '<th>' + (1/params.gamma).toFixed(2) + '</th>' +
             '<th>' + mse.toFixed(3) + '</th>' +
             '</tr>';
    table += '<tr class="accordion collapse">' +
             '<th>Best</th>  <th>' + bestScore['direct-contact'].modelName + '</th>' +
             '<th>' + bestScore['direct-contact'].R0 + '</th>' +
             '<th>' + (1/bestScore['direct-contact'].gamma).toFixed(2) + '</th>' +
             '<th>' + bestScore['direct-contact'].mse.toFixed(3) + '</th>' +
             '</tr>';
    if(bestScore['vector-borne'].modelName){
         table += '<tr class="accordion collapse">' +
                  '<th>Best</th>  <th>' + bestScore['vector-borne'].modelName + '</th>' +
                  '<th>' + bestScore['vector-borne'].R0 + '</th>' +
                  '<th>' + (1/bestScore['vector-borne'].gamma).toFixed(2) + '</th>' +
                  '<th>' + bestScore['vector-borne'].mse.toFixed(3) + '</th>' +
                  '</tr>';
    }

    if(bestScore['latent-stage'].modelName){
         table += '<tr class="accordion collapse">' +
                  '<th>Best</th>  <th>' + bestScore['latent-stage'].modelName + '</th>' +
                  '<th>' + bestScore['latent-stage'].R0 + '</th>' +
                  '<th>' + (1/bestScore['latent-stage'].gamma).toFixed(2) + '</th>' +
                  '<th>' + bestScore['latent-stage'].mse.toFixed(3) + '</th>' +
                  '</tr>';
    }
    table += '</tbody>';
    $('#param-values').html(table);

}


/*
$('#inputr0').slider({
	formatter: function(value) {
		return 'R0: ' + value;
	}
}).on('change', function(slideEvt){
  params.R0 = slideEvt.value.newValue;
$('#inputr0-label').text('R0: '+params.R0.toFixed(2));
  var modelOutput = params.model(params,maxT);
  plotGraph('SIRGraphDiv',modelOutput);
  updateStatistics(params,modelOutput);
});
*/

document.getElementById("range-inputr0").oninput = function() {
  params.R0 = this.value/100;
  $('#inputr0-label').text('R0: '+params.R0.toFixed(2));
  plotAllGraphs();
  //updateStatistics(params,modelOutput);
}
/*
$('#inputgamma').slider({
	formatter: function(value) {
		return 'rate of recovery: ' + value + ' days';
	}
}).on('change',function(slideEvt){
  params.gamma = 1/slideEvt.value.newValue;
  modelOutput = params.model(params,maxT);
  $('#inputgamma-label').text('Infectious period: '+ slideEvt.value.newValue.toFixed(2) + ' days');
  plotGraph('SIRGraphDiv',modelOutput);
  updateStatistics(params,modelOutput);
});
*/

document.getElementById("range-inputgamma").oninput = function() {
    params.gamma = 10/this.value;
    $('#inputgamma-label').text('Infectious period: '+ (1/params.gamma).toFixed(2) + ' days');
    plotAllGraphs();
    //updateStatistics(params,modelOutput);
}

document.getElementById("range-inputcoverage").oninput = function() {
    params.coverage = this.value/100;
    $('#inputcoverage-label').text('Vaccination coverage: '+ (100*params.coverage).toFixed(0) + '%');
    plotAllGraphs();

    //updateStatistics(params,modelOutput);
}

$('#modelSelect').on('change', function() {
    if(this.value=='direct'){
        params.modelName = 'direct-contact';
        params.model = simpleSIR;
    } else if (this.value=='vector'){
        params.modelName = 'vector-borne';
        params.model = vectorSIR;
    } else if (this.value=='latent'){
        params.modelName = 'latent-stage';
        params.model = SEIR;

    } else {
        params.modelName = 'frequency';
        params.model = frequencySIR;
    }
    modelOutput = params.model(params,maxT);
    plotGraph('SIRGraphDiv',modelOutput);
    //updateStatistics(params,modelOutput);
})



window.onload = function () {
    params.modelName = 'direct-contact';
    params.model = simpleSIR;

    var output = simpleSIR(sparams,maxT);
    /*
    data.I = [];
    for (var i = 0; i < output.I.length; i++){
        if((i%Math.floor(1/sparams.dt))==0){
            data.I.push(poisson(output.I[i]));
        }
    }
    */
    //data = simpleStochasticSIR(sparams,maxT,true);
    modelOutput = params.model(params,maxT);
    plotGraph('SIRGraphDiv',modelOutput);
    plotTotalGraph('totalGraphDiv',modelOutput,modelOutput);
    //updateStatistics(params,modelOutput);


    //plotStochGraph('StochSIRGraphDiv',multipleStochsticSIR(sparams,maxT,nreps));
}

$(document).ready(function(){
    $('[data-toggle="tooltip"]').tooltip();
});
