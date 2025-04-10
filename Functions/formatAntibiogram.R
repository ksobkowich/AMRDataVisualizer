classicAB <- function(data, obs_cols, drug_targets, combined_js, height){
  
  datatable(
    data,
    rownames = FALSE,
    class = "cell-border",
    extensions = "FixedColumns",
    options = list(
      autoWidth = FALSE,
      scrollX = TRUE,
      scrollY = height,
      scrollCollapse = TRUE,
      fixedHeader = TRUE,
      dom = 't',
      fixedColumns = list(leftColumns = 2),
      paging = FALSE,
      ordering = FALSE,
      
      columnDefs = list(
        list(targets = obs_cols - 1, visible = FALSE),
        list(targets = drug_targets, createdCell = combined_js),
        
        list(
          targets = 0,
          createdCell = JS("
      function(td, cellData, rowData, row, col) {
        $(td).css({
          'width': '180px',
          'white-space': 'nowrap',
          'overflow': 'hidden',
          'text-overflow': 'ellipsis'
        });
      }
    ")
        ),
        
        list(
          targets = 1,
          createdCell = JS("
      function(td, cellData, rowData, row, col) {
        $(td).css({
          'width': '120px',
          'white-space': 'nowrap',
          'overflow': 'hidden',
          'text-overflow': 'ellipsis'
        });
      }
    ")
        )
        
      ),
   
      headerCallback = JS(
        "function(thead, data, start, end, display) {",
        "  function rotateHeaders($ths) {",
        "    var betterCells = [];",
        "    $ths.each(function(index) {",
        "      var cell = $(this);",
        "      if (index === 0 || index === 1) {",
        "        betterCells.push(cell.html());",
        "      } else {",
        "        var newDiv = $('<div>', {style: 'height: auto; width: 10px; transform: rotate(-90deg); white-space: nowrap;'});",
        "        var newInnerDiv = $('<div>', {text: cell.text()});",
        "        newDiv.append(newInnerDiv);",
        "        betterCells.push(newDiv);",
        "      }",
        "    });",
        "    $ths.each(function(i) {",
        "      $(this).html(betterCells[i]);",
        "    });",
        "    $ths.css({",
        "      'vertical-align': 'bottom',",
        "      'text-align': 'center',",
        "      'height': '120px'",
        "    });",
        "  }",
        "  rotateHeaders($(thead).find('th'));",
        "  rotateHeaders($('.DTFC_Cloned thead th'));",
        "}"
      ),
      
      drawCallback = JS(
        "function(settings) {",
        "  var api = this.api();",
        "  setTimeout(function() { api.columns.adjust().draw(); }, 100);",
        "}"
      )
    )
    
  )
  
}