function zMap = renameProbMaps(inputImage,opt)
% reads the probability images and renames them with bids rules

p.filename = spm_file(inputImage, 'filename');
p.suffix = 'probseg';
p.ext = '.nii';
p.entities.space = 'MNI';

basename = spm_file(inputImage, 'basename');
parts = strsplit(basename, '_');

p.entities.label = [parts{3} parts{1} parts{2}];
p.use_schema = false;

newName = bids.create_filename(p);

outputImage = spm_file(inputImage, 'filename', newName);

movefile(inputImage, outputImage);

zMap = outputImage;

end