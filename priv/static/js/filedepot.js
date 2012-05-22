$(document).ready(function() {

	$("#files tbody tr").click(function() {
		console.log("Clicked!");
		if ($(this).next().hasClass("preview")) {
			$(this).next().toggle();
		}
		else {
			var target = $(this);
			var url = $(this).find('a').attr('href');
			// Fetch the file contents from the given url
			$.get(url, function(ret) {

			}).complete(function(jqXHR, textStatus) {
				// When request has completed,
				// let's display the contents depending on the content-type
				var contentType = jqXHR.getResponseHeader('Content-Type');
				var data = jqXHR.responseText;
				// If it's an image, display content in an <img> tag
				if (contentType.split('/')[0] == "image") {
					var img = '<img src="' + url + '" />';
					target.after('<tr class="preview"><td colspan="3">' + img + '</td></tr>');
				}
				// Otherwise, print contents
				else {
					switch (contentType) {
						case "text/plain":
							output = data;
							break;
						default:
							output = "Preview for this filetype not supported yet. Click the filename to preview in a new browser tab.";
							break;
					}
					// Make sure newlines are printed
					output = output.replace(/\n/g, "<br>");
					// Insert the data
					target.after('<tr class="preview"><td colspan="3">' + output + '</td></tr>');
				}
			});
		}
	}); // End #files click

	var dropbox = $('#dropbox'),
			message = $('.message', dropbox);

	dropbox.filedrop({
		// The name of the files array
		paramname: 'files',

		maxfiles: 5,
		maxfilesize: 5, // in megabytes
		url: dropbox.attr('data-target'),

		// file is a File object containing 
		// 	lastModifiedDate, name, size, webkitRelativePath
		// response is the path to the temporarily stored file

		uploadFinished: function(i, file, response) {			

			// If the save button isn't visible, show it
			if ($("#saveButton").hasClass('hidden'))
				$("#saveButton").show();
			console.log(response);

			// console.log(file);
			// console.log(typeof(file));
			// var filename = file.name;
			// var tempPath = response.tempPath;
			// previewFile(filename, tempPath);
			// Make sure we got an actual Object back, containing file information
			// if (typeof(response) == "object") {
			// 	$(".preview").append(file);
			// }
		},

		error: function(err, file) {
			switch (err) {
				case 'BrowserNotSupported':
					showMessage('Your browser does not support HTML5 file uploads!');
					break;
				case 'TooManyFiles':
					showWarning('Too many files! Please select 5 at most!');
					break;
				case 'FileTooLarge':
					showWarning('\'' + file.name + '\' is too large! Max size allowed 5mb');
					break;
				default:
					break;
			}
		},

		// Called before each upload is started
		beforeEach: function(file){

			// if(!file.type.match(/^image\//)){
			// 				alert('Only images are allowed!');
			//
			// 				// Returning false will cause the
			// 				// file to be rejected
			// 				return false;
			// 			}
		},

		uploadStarted:function(i, file, len){								
			previewFile(file.name, file.size);			
		},

		progressUpdated: function(i, file, progress) {
			console.log("progressUpdated");
			console.log(progress);
			console.log(file);
			updateProgressBar(file.name, progress);
			// $.data(file).find('.progress').width(progress);
		}
	});

	// After we've uploaded our files, we want to save them to the server
	$("#saveButton").click(function() {
		$("#saveAlert").html("Files saved!");
		$("#saveAlert").show();
	});

	function showWarning(msg) {
		message.addClass('warning');
		message.html(msg);
	}

	function showMessage(msg){
		message.html(msg);
	}

	// This function updates the progress bar belonging to 'filename' to 'progress'
	function updateProgressBar(filename, progress) {
		// Apply a filter only to keep the one row that contains 'filename'
		var tr = $("#preview tbody tr").filter(function(i) {									
			return $('td.filename', this).html() == filename;			
		});
		// Set the progress bar to 'progress' %		
		$('.progress .bar', tr).css('width', progress+'%');
		// Remove the 'active' animation if the progress is updated to 100%
		if (progress == 100)
			$('.progress', tr).removeClass('active');
	}

	function previewFile(filename, size) {
		var target = $("#preview");
		var tbody = target.find("tbody");

		// Structure for the progress bar
		var progress = 
		'<div class="progress progress-striped active">' +
  		'<div class="bar" style="width: 0%;"></div>' +
		'</div>'

		// Structure for the addition/removal of tags
		var tags = '<a href="#">edit tags</a>';

		var template = 
		'<tr>'	+	
			'<td class="filename">' + filename + '</td>' +
			'<td>' + fileSizeFormat(size) +'</td>' +
			'<td>' + progress +'</td>' +
			'<td>' + tags +'</td>' +
		'</tr>';

		tbody.append(template);
	}

	// Format the value like a human-readable file size
	var FD_KILOBYTE = 1024;
	var FD_MEGABYTE = FD_KILOBYTE * 1024;
	var FD_GIGABYTE = FD_MEGABYTE * 1024;

	function fileSizeFormat(size) {			
		if (size >= FD_GIGABYTE)
			return fileSizeFormat(size/FD_GIGABYTE) + " GB";
		else if (size >= FD_MEGABYTE)
			return fileSizeFormat(size/FD_MEGABYTE) + " MB";
		else if (size >= FD_KILOBYTE)
			return fileSizeFormat(size/FD_KILOBYTE) + " KB";
		else if (typeof(size) == "number")
			return Math.round(size*100)/100;
		else 
			return size + " bytes";
	}	
});