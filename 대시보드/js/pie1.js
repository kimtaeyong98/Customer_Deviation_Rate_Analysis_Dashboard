result1 = 9196/(9196+4644)*100;
result2 = 4644/(9196+4644)*100;
var data = [
    {name: "해결됨", value: 9196, ratio: result1.toFixed(2)},
    {name: "미해결", value: 4644, ratio: result2.toFixed(2)},
  ];

  var text = "";
  
  var width = 250;
  var height = 250;
  var thickness = 40;
  var duration = 750;
  var padding = 10;
  var opacity = .8;
  var opacityHover = 1;
  var otherOpacityOnHover = .8;
  var tooltipMargin = 13;
  
  var radius = Math.min(width-padding, height-padding) / 2;
  var color = d3.scaleOrdinal()
    .range(["#acff6e","#d47afe"]);
  
  var svg1 = d3.select("#row2_3")
  .append('svg')
  .attr('class', 'pie')
  .attr('width', width)
  .attr('height', height);
  
  var g = svg1.append('g')
  .attr('transform', 'translate(' + (width/2) + ',' + (height/2) + ')');
  
  var arc = d3.arc()
  .innerRadius(0)
  .outerRadius(radius);
  
  var pie = d3.pie()
  .value(function(d) { return d.value; })
  .sort(null);
  
  var path = g.selectAll('path')
    .data(pie(data))
    .enter()
    .append("g")  
    .append('path')
    .attr('d', arc)
    .attr('fill', (d,i) => color(i))
    .style('opacity', opacity)
    .style('stroke', 'white')
    .on("mouseover", function(d) {
        d3.select(this)
          .style("fill","red")

        let g = d3.select("#row2_3")
          .style("cursor", "pointer")
          .append("g")
          .attr("class", "tooltip")
          .style("opacity", 0);
   
        g.append("text")
          .attr("class", "name-text")
          .attr("id","text5")
            d3.select("#text5")           
            .text(`${d.data.name} (${d.data.value+"건"}, ${d.data.ratio+"%"})`)
            .attr('text-anchor', 'middle')         

        let text = g.select("text");
        let bbox = text.node().getBBox();
        let padding = 2;
        g.insert("rect", "text")
          .attr("x", bbox.x - padding )
          .attr("y", bbox.y - padding)
          .attr("width", bbox.width + (padding*2))
          .attr("height", bbox.height + (padding*2))
          .style("fill", "white")
          .style("opacity", 0.75);
      })
    .on("mousemove", function(d) {
          let mousePosition = d3.mouse(this);
          let x = mousePosition[0] + width/2;
          let y = mousePosition[1] + height/2 - tooltipMargin;
      
          let text = d3.select('.tooltip text');
          let bbox = text.node().getBBox();
          if(x - bbox.width/2 < 0) {
            x = bbox.width/2;
          }
          else if(width - x - bbox.width/2 < 0) {
            x = width - bbox.width/2;
          }
      
          if(y - bbox.height/2 < 0) {
            y = bbox.height + tooltipMargin * 2;
          }
          else if(height - y - bbox.height/2 < 0) {
            y = height - bbox.height/2;
          }
      
          d3.select('.tooltip')
            .style("opacity", 1)
            .attr('transform',`translate(${x}, ${y})`);
      })
    .on("mouseout", function(d) {   
        d3.select(".tooltip")
          .style("opacity", 0)  
          .select(".tooltip").remove();
        d3.selectAll('path')
          .style("opacity", opacity);
        d3.select(this)
          .style("fill",function(d,i){return color[d];})
      })
    .on("touchstart", function(d) {
        d3.select("svg")
          .style("cursor", "none");    
    })
    .each(function(d, i) { this._current = i; });
  
  let legend = d3.select("#chart").append('div')
              .attr('class', 'legend')
              .style('margin-top', '30px');

    svg5=d3.select("#row2_3")
    svg5.append("rect").attr("x",220).attr("y",230).attr("width", 18).attr("height", 18).style("fill", "rgb(212,122,254)")
    svg5.append("rect").attr("x",220).attr("y",250).attr("width", 18).attr("height", 18).style("fill", "rgb(172,255,110)")
    svg5.append("text").attr("x", 240).attr("y", 240).text("미해결").style("font-size", "15px").attr("alignment-baseline","middle").attr("font-family", "'Jua', sans-serif")
    svg5.append("text").attr("x", 240).attr("y", 260).text("해결됨").style("font-size", "15px").attr("alignment-baseline","middle").attr("font-family", "'Jua', sans-serif")